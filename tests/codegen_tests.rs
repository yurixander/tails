extern crate inkwell;
extern crate tails;

#[cfg(test)]
mod tests {
  use pretty_assertions::assert_eq;
  use tails::{diagnostic, lexer, pass};

  const TESTS_FOLDER: &str = "tests";
  const BUG_CURRENT_FOLDER: &str = "the current directory should exist and be accessible";

  const BUG_FILE_READ: &str =
    "test source file should exist, be accessible, and its contents should be valid UTF-8";

  fn lex_and_filter(source_code: &str) -> diagnostic::Maybe<Vec<lexer::Token>> {
    let tokens = tails::lexer::Lexer::lex_all(source_code)?;

    // SAFETY: What about illegal tokens? Would it cause the parser to error?
    // Filter tokens to only include those that are relevant (ignore
    // whitespace, comments, etc.).
    let filtered_tokens = tokens
      .into_iter()
      .filter(|token| {
        !matches!(
          token.0,
          tails::lexer::TokenKind::Whitespace(_) | tails::lexer::TokenKind::Comment(_)
        )
      })
      .collect();

    Ok(filtered_tokens)
  }

  fn lower_file(
    source_file_contents: &str,
    qualifier: tails::symbol_table::Qualifier,
  ) -> diagnostic::Maybe<String> {
    let mut parser = tails::parser::Parser::new(lex_and_filter(source_file_contents)?);
    let module_result = parser.parse_module(qualifier.clone());

    let module = match module_result {
      Ok(unit) => unit,
      Err(diagnostics) => return Err(diagnostics),
    };

    let test_package = tails::ast::Package::from([(qualifier.clone(), module)]);
    let mut pass_manager = pass::PassManager::new(&test_package);

    pass_manager.add_all_passes(qualifier);

    let pass_manager_run_result = pass_manager.run(parser.get_id_count());

    let diagnostics_helper = diagnostic::DiagnosticsHelper {
      diagnostics: pass_manager_run_result.diagnostics,
    };

    if diagnostics_helper.contains_errors() {
      return Err(diagnostics_helper.diagnostics);
    }

    let llvm_lowering_pass_result = pass_manager_run_result
      .results
      .get(&pass::PassId::LlvmLowering)
      .expect("backend output should have been produced if there were no error diagnostics");

    Ok(match llvm_lowering_pass_result {
      // OPTIMIZE: Consume result and avoid cloning.
      pass::PassResult::LlvmIrOutput(llvm_ir_output) => llvm_ir_output.to_owned(),
      _ => {
        unreachable!("backend output should have been produced if there were no error diagnostics")
      }
    })
  }

  fn run_test(name: &str, folder_name: &str) -> diagnostic::Maybe<String> {
    // CONSIDER: Actually compiling the output LLVM IR and running it, and expecting a `0` return code.

    const FILENAME_EXTENSION: &str = "tails";

    let tests_path = std::env::current_dir()
      .expect(BUG_CURRENT_FOLDER)
      .join(TESTS_FOLDER);

    let source_file_contents = std::fs::read_to_string(
      tests_path
        .join(folder_name)
        .join(name)
        .with_extension(FILENAME_EXTENSION),
    )
    .expect(BUG_FILE_READ);

    let qualifier = tails::symbol_table::Qualifier {
      package_name: String::from(TESTS_FOLDER),
      // FIXME: File names need to conform to identifier rules.
      module_name: name.to_string(),
    };

    lower_file(&source_file_contents, qualifier).map(|output| output.trim().to_string())
  }

  fn run_passing_test(name: &str) {
    const LLVM_FILENAME_EXTENSION: &str = "ll";
    const EXPECTED_OUTPUT_FOLDER: &str = "expected_output";
    const INPUT_FOLDER: &str = "passing";

    let tests_path = std::env::current_dir()
      .expect(BUG_CURRENT_FOLDER)
      .join(TESTS_FOLDER);

    let tests_output_path = tests_path.join(EXPECTED_OUTPUT_FOLDER);

    let output_file_path = tests_output_path
      .join(name)
      .with_extension(LLVM_FILENAME_EXTENSION);

    let actual_output = run_test(name, INPUT_FOLDER)
      .expect("there should be no error diagnostics produced on a passing test");

    let expected_output = if output_file_path.exists() {
      std::fs::read_to_string(output_file_path)
        .expect("corresponding output file exists, but cannot be read")
        .trim()
        .to_string()
    }
    // If the expected output file does not exist, that is acceptable;
    // the output LLVM IR is irrelevant. For example, this could mean that
    // the test did not aim to test LLVM lowering directly, but instead
    // things like the type system, or earlier phases.
    else {
      return;
    };

    assert_eq!(expected_output, actual_output);
  }

  // TODO: Add support for specifying what exactly should be wrong with the test (via diagnostics).
  fn run_failing_test(name: &str, matcher: &dyn Fn(Vec<diagnostic::Diagnostic>) -> bool) {
    const FAILING_FOLDER: &str = "failing";

    match run_test(name, FAILING_FOLDER) {
      Ok(llvm_ir_output) => {
        println!("{}", llvm_ir_output);
        panic!("failing tests should not succeed");
      }
      Err(diagnostics) => {
        let matcher_result = matcher(diagnostics.clone());

        if !matcher_result {
          dbg!(diagnostics);
        }

        assert!(
          matcher_result,
          "failing test should produce expected diagnostics"
        );
      }
    }
  }

  macro_rules! define_passing_tests {
    ($($name:ident),* $(,)?) => {
      $(
        #[test]
        fn $name() {
          run_passing_test(stringify!($name));
        }
      )*
    };
  }

  macro_rules! define_failing_tests {
    ($($name:ident),* $(,)?) => {
      $(
        #[test]
        fn $name() {
          run_failing_test(stringify!($name), &|diagnostics| !diagnostics.is_empty());
        }
      )*
    };
  }

  // CONSIDER: Adding a `integration` folder, which contains tests that are more complex and involve the usage of multiple language constructs in conjunction. Then, that folder's files would be automatically picked up by the test runner, and if Rust allows it, all the tests would be run, without comparison against expected output (ie. only perform LLVM module verification). If Rust doesn`t allow `dynamic` test creation, just add a macro similar to `define_passing_tests!` (would need to manually list the tests located in the `integration` directory).

  define_passing_tests!(
    access,
    access_object_string,
    access_foreign_var,
    as_int_to_int,
    as_int_to_real,
    binary_op_arithmetic,
    binary_op_logical,
    binding,
    binding_hof,
    binding_literal,
    binding_nullptr,
    binding_unit,
    block,
    block_yield_binding,
    closure,
    closure_capture_binding,
    closure_capture_parameter,
    closure_capture_generic_parameter,
    closure_capture_self_calling,
    closure_capture_object,
    closure_capture_multiple,
    closure_capture_return_closure,
    closure_type_hint_constraint,
    // closure_binding_no_redefine,
    closure_self_call,
    closure_return,
    constant,
    effect_simple,
    // factorial,
    // fibonacci,
    foreign,
    foreign_var,
    foreign_var_object_type,
    foreign_varargs,
    function_empty,
    function_param,
    function_return,
    generics_call_zero,
    generics_call_complex,
    generics_call_object,
    generics_call_id,
    generics_call_id_single_call,
    generics_call_memo,
    generics_call_multi_annotations,
    generics_call_multi_artifacts,
    generics_call_chain,
    generics_closure_indirect_usage,
    generics_type_def,
    generics_type_def_unused,
    generics_type_def_id,
    generics_type_def_wrapper,
    generics_type_def_nesting,
    generics_type_def_multi_step,
    generics_type_def_complex,
    generics_type_def_same_multi_use,
    generics_unused,
    generics_sizeof,
    guard_division_by_zero,
    guard_memo,
    guard_null_dereference,
    hof,
    hof_args,
    hof_return,
    if_,
    if_else,
    if_elif,
    if_elif_else,
    if_values,
    if_nesting,
    if_nested_condition,
    if_complex_condition,
    infer_binary_op,
    infer_signature,
    sizeof,
    literals,
    // loop_,
    // loop_closure,
    // loop_range,
    declare,
    name_tick,
    object,
    object_nested,
    object_field_shorthand,
    object_call_pass_binding,
    playground,
    pipe,
    pipe_chain,
    pointer,
    pointer_assignment,
    pointer_assignment_foreign,
    pointer_index,
    match_,
    reference,
    reference_object,
    simple_program,
    tuple_typed,
    tuple_nested,
    tuple_single,
    type_infer_binding,
    type_infer_parameter,
    type_infer_object_access,
    type_infer_return_type,
    type_infer_complex,
    type_def,
    type_def_nested,
    unary_op,
    union,
    unit_constant,
    unit_if,
    unit_parameter_mixed,
    unit_parameter_multiple,
    unit_parameter_single,
    unit_object_fields,
    unit_closure_capture,
    vector,
    vector_generics,
    // recursion_direct,
    recursion_mutual,
    recursion_trio,
    lex_unicode_string
  );

  define_failing_tests!(
    parser_function_missing_name,
    parser_trailing_comma,
    lex_unicode_unsupported,
    // FIXME: This is passing, but for the wrong reason. Logic to properly handle this case still needs to be implemented. See the test for more details.
    generics_call_param_illegal_usage,
    generics_hints_missing,
    generics_hints_mismatch,
    generics_hints_count_mismatch,
    // generics_type_def_recursive,
    type_def_recursive,
    type_def_recursive_usage,
    type_def_mutually_recursive,
    type_def_mutually_recursive_usage,
    type_def_generics_recursive,
    // type_def_generics_recursive_usage,
    type_def_generics_mutually_recursive,
    type_def_generics_mutually_recursive_usage,
    type_def_recursive_nested,
    call_argument_count,
    reference_return,
    object_missing_field,
    constant_runtime_value,
    declare_parameter_redefine,
    declare_parameter_redefine_function,
    declare_generic_redefine,
    declare_binding_redefine,
    declare_function_redefine,
    declare_foreign_function_redefine,
    call_site_invalid_direct_callee,
    call_site_invalid_indirect_callee,
    resolution_missing_function,
    type_infer_mismatch
  );
}

// CONSIDER: Add embedded special-comment syntax checking feature to check produced LLVM IR, or certain properties of things. This will require that comments are treated as AST items, and also would help quite a lot if they are somehow 'attached' to things next to it; ability to obtain what comments are attached to.

// TODO: Add new test module: "run-pass", should run the executable produced and complete without any panics. Also, "run-fail" tests. Furthermore, add tests around issues already encountered: specifically designed tests to ensure that fixed bugs do not reoccur, and finally, consider benchmark tests. Moreover, the comment syntax can also extend to runnable tests: by providing all the different inputs that the main function would receive and be executed on. Get creative; it's powerful.

// CONSIDER: Static assert functionality, which can actually be used within tests to STATICALLY (without running) perform assertions on constants. Also, consider constexpr functions.
