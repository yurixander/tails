use crate::{
  ast, auxiliary, inference, instantiation, lowering, resolution, symbol_table, types, unification,
  visit::Visitor,
};
use inkwell::{types::BasicType, values::BasicValue};

pub(crate) const BUG_TYPE_NEVER_UNIT: &str = "type should never be unit";
pub(crate) const BUG_LLVM_VALUE: &str = "should always yield an LLVM value";

const BUG_INSTANTIATION: &str =
  "a corresponding instantiation should exist, and the universe id should be valid";

// NOTE: Current universe id is not saved, because it is already handled
// in a stack-like manner by the `lower_artifact` utility function.
pub struct LoweringState<'llvm> {
  llvm_current_block: Option<inkwell::basic_block::BasicBlock<'llvm>>,
  llvm_current_function: Option<inkwell::values::FunctionValue<'llvm>>,
  llvm_entry_block: Option<inkwell::basic_block::BasicBlock<'llvm>>,
  access_mode: AccessMode,
  runtime_guards_failure_buffers:
    std::collections::HashMap<RuntimeGuard, inkwell::basic_block::BasicBlock<'llvm>>,
}

#[derive(Hash, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeGuard {
  NullDereference,
  DivisionByZero,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum AccessMode {
  /// Do not access any LLVM value. This is equivalent to an `lvalue` in
  /// other languages,
  None,
  /// Access all values, regardless of whether they are pointers or
  /// strings.
  All,
  /// Only access pointer values. Do not access any other LLVM value,
  /// including strings.
  PointersOnly,
  /// Access all LLVM values (including pointers), except for strings.
  ExcludeStrings,
  /// Exclude strings and pointers from being accessed. Most values will
  /// be lowered with this access rule. This is equivalent to an `rvalue`
  /// in other languages.
  Value,
  /// A dereference operation is in place. Pointer values should be dereferenced.
  Dereference,
}

#[derive(Clone)]
pub(crate) struct MonomorphismCacheEntry<'llvm>(
  pub Vec<types::Type>,
  pub inkwell::values::FunctionValue<'llvm>,
);

type MonomorphismCache<'llvm> =
  std::collections::HashMap<symbol_table::RegistryId, Vec<MonomorphismCacheEntry<'llvm>>>;

// CONSIDER: Adding `get` functions to avoid giving read-write access to properties by having them be `pub`.
pub struct LoweringContext<'a, 'llvm> {
  pub(crate) llvm_builder: inkwell::builder::Builder<'llvm>,
  /// Represents the active LLVM function value currently being lowered.
  ///
  /// This buffer exists to aid other context-sensitive nodes during lowering.
  pub(crate) llvm_function_buffer: Option<inkwell::values::FunctionValue<'llvm>>,
  pub(crate) symbol_table: &'a symbol_table::SymbolTable,
  pub(crate) llvm_module: &'a inkwell::module::Module<'llvm>,
  // FIXME: The current memoization approach is better than not considering the access mode, however there's a crucial flaw that must be addressed: If the value is lowered with different access modes, it will be lowered various times, regardless of whether the different access mode affected the lowering of the value or not! Immediate issues with this could be redundant duplicated LLVM IR. However, it could be perhaps optimized away by LLVM.
  pub(crate) llvm_value_memoization: std::collections::HashMap<
    (AccessMode, symbol_table::RegistryId),
    Option<inkwell::values::BasicValueEnum<'llvm>>,
  >,
  pub(crate) resolution_helper: &'a resolution::ResolutionHelper<'a>,
  /// Represents the entry block of the current function buffer, if any.
  ///
  /// Used to place alloca instructions for optimization purposes.
  pub(crate) llvm_entry_block: Option<inkwell::basic_block::BasicBlock<'llvm>>,
  /// A cache of monomorphized (specialized) polymorphic functions.
  ///
  /// This is used to avoid re-specializing the same function multiple times. In
  /// other words, this is used for memoization (an optimization technique).
  ///
  /// When a call expression attempts to specialize a polymorphic function, if the
  /// same monomorphism configuration is used (i.e. the same concrete type arguments
  /// are being used to fill in the generic types) has been previously used, then such
  /// result is to be re-used instead of re-specializing the function.
  pub(crate) monomorphism_cache: MonomorphismCache<'llvm>,
  pub(crate) universe_stack: resolution::UniverseStack,
  /// A flag that indicates how to treat or perform access (load) on certain nodes.
  ///
  /// Access is a term that describes the process of stripping a pointer layer from
  /// an LLVM value, by using a `load` instruction.
  ///
  /// Certain nodes need to be accessed implicitly, such as when a variable declaration
  /// is referenced. However, there are exceptions, for example string pointers are not
  /// to be accessed, because otherwise the string value would be lost and it would return
  /// a single character (`i8`).
  ///
  /// When LLVM values that are behind pointer layers are to be used as values for an
  /// operation, for example, addition, they must be accessed to have that layer stripped.
  pub(crate) access_mode: AccessMode,
  /// Contains a mapping of interned string literals to their LLVM value.
  ///
  /// This serves as a cache to avoid re-creating the same string literal multiple times.
  interned_string_literals:
    std::collections::HashMap<String, inkwell::values::BasicValueEnum<'llvm>>,
  qualifier: symbol_table::Qualifier,
  runtime_guards_failure_buffers:
    std::collections::HashMap<RuntimeGuard, inkwell::basic_block::BasicBlock<'llvm>>,
}

impl<'a, 'llvm> LoweringContext<'a, 'llvm> {
  pub(crate) const LLVM_CONST_ADDRESS_SPACE: u16 = 4;

  /// Perform a primitive cast that should succeed.
  ///
  /// This is useful for when performing truncate cast operations between
  /// primitive number types, since LLVM's API is picky on the bit-widths of
  /// integers that some of its functions accept.
  ///
  /// ## Panics
  ///
  /// If the cast cannot succeed, then it is assumed that a logic bug is present, and
  /// a panic will be issued. This assumption stems from relying on the parser to check
  /// lengths or sizes of parsed elements or values before proceeding to subsequent
  /// passes.
  pub(crate) fn assert_trunc_cast<U, T: std::convert::TryFrom<U>>(value: U) -> T {
    T::try_from(value).unwrap_or_else(|_| {
      panic!("truncating cast of primitive type could not be completed without discarding data")
    })
  }

  pub(crate) fn find_bit_width(ty: &types::Type) -> Option<types::BitWidth> {
    match ty {
      types::Type::Primitive(primitive_type) => match primitive_type {
        types::PrimitiveType::Integer(bit_width, ..) => Some(*bit_width),
        types::PrimitiveType::Real(bit_width, ..) => Some(*bit_width),
        _ => None,
      },
      _ => None,
    }
  }

  pub(crate) fn new(
    qualifier: symbol_table::Qualifier,
    symbol_table: &'a symbol_table::SymbolTable,
    resolution_helper: &'a resolution::ResolutionHelper<'a>,
    llvm_module: &'a inkwell::module::Module<'llvm>,
  ) -> Self {
    // CONSIDER: Returning a `Result` instead of panicking, since the LLVM module is GIVEN, and this function should be blind to outside state? Currently it assumes that if a given LLVM module is bad, it constitutes a logic bug.
    // Ensure that the given LLVM module is valid.
    assert!(
      llvm_module.verify().is_ok(),
      "the given LLVM module should be valid"
    );

    Self {
      qualifier,
      symbol_table,
      llvm_module,
      llvm_builder: llvm_module.get_context().create_builder(),
      llvm_function_buffer: None,
      llvm_value_memoization: std::collections::HashMap::new(),
      access_mode: AccessMode::None,
      llvm_entry_block: None,
      resolution_helper,
      monomorphism_cache: MonomorphismCache::new(),
      interned_string_literals: std::collections::HashMap::new(),
      runtime_guards_failure_buffers: std::collections::HashMap::new(),
      universe_stack: resolution::UniverseStack::new(),
    }
  }

  pub(crate) fn memoize_monomorphic_fn(
    &mut self,
    id: symbol_table::RegistryId,
    call_argument_types: Vec<types::Type>,
    llvm_function: inkwell::values::FunctionValue<'llvm>,
  ) {
    let value = MonomorphismCacheEntry(call_argument_types, llvm_function);

    self
      .monomorphism_cache
      .entry(id)
      // OPTIMIZE: Avoid cloning.
      .and_modify(|entry| entry.push(value.clone()))
      .or_insert(vec![value]);
  }

  pub(crate) fn find_memoized_monomorphism(
    &self,
    function_id: &symbol_table::RegistryId,
    argument_types: &[types::Type],
  ) -> Option<inkwell::values::FunctionValue<'llvm>> {
    let monomorphisms = self.monomorphism_cache.get(function_id)?;

    // OPTIMIZE: This operation is `O(n)`, where `n` represents the amount of monomorphisms for any given polymorphic function (could be a lot!).
    for monomorphism_types in monomorphisms {
      // BUG: (tag:object-unification-cache) It seems that unifying object types causes false positives, and thus monomorphisms are cached when they shouldn't be. Then, retrieved with differing object type argument/parameter sets when they shouldn't; which seems to be caused by misleading object type unification!
      if self.unify_type_sets(&monomorphism_types.0, argument_types) {
        return Some(monomorphism_types.1);
      }
    }

    None
  }

  /// An utility function that uses unification to determine whether
  /// two slices of types are equivalent.
  fn unify_type_sets(&self, set_a: &[types::Type], set_b: &[types::Type]) -> bool {
    // CONSIDER: Moving this functionality under the `types` module.
    // TODO: Check that provided types are concrete (as they should be by this stage), as generic types cannot be unified (they would just be ignored, and may return a false positive result). If so, consider changing the return type of this function to be more explicit, by making it an enum, such as `UnificationResult::CannotUnifyGenericTypes` and `UnificationResult::Success`.

    if set_a.len() != set_b.len() {
      return false;
    }

    let universes = instantiation::TypeSchemes::new();

    let mut unification_ctx = unification::TypeUnificationContext::new(
      self.symbol_table,
      symbol_table::SubstitutionEnv::new().to_owned(),
      &universes,
    );

    let constraints = set_a
      .iter()
      .zip(set_b.iter())
      .map(|(monomorphism_type, given_type)| {
        inference::Constraint::Equality(monomorphism_type.to_owned(), given_type.to_owned())
      })
      .map(|constraint| (resolution::UniverseStack::new(), constraint))
      .collect::<Vec<_>>();

    // REVIEW: Passing-in an empty type environment is expected?
    // If the unification process succeeded without any error diagnostics,
    // then all the types were successfully unified.
    unification_ctx
      .solve_constraints(&symbol_table::TypeEnvironment::new(), &constraints)
      .is_ok()
  }

  /// Inserts an LLVM alloca instruction in the entry basic block of the
  /// current function buffer.
  ///
  /// Since alloca instructions not located in the entry basic block are
  /// treated as dynamic allocations, optimization opportunities would
  /// be lost if they are placed in arbitrary basic blocks.
  ///
  /// ## Panics
  ///
  /// This function assumes that the LLVM entry block buffer has been set.
  /// If unset, a panic will occur when unwrapping the buffer.
  pub(crate) fn alloca(
    &mut self,
    llvm_type: impl Into<inkwell::types::BasicTypeEnum<'llvm>>,
    name: &str,
  ) -> inkwell::values::PointerValue<'llvm> {
    // By placing alloca instructions on the entry block, LLVM's mem2reg pass
    // will promote them to SSA form, which provides LLVM with optimization
    // opportunities.
    let llvm_entry_block = self.llvm_entry_block.expect(auxiliary::BUG_BUFFER_CONTRACT);

    // By using a new builder, issues with positioning and possible displacement
    // of the LLVM builder buffer are avoided.
    let llvm_builder = self.llvm_module.get_context().create_builder();

    // Position at the beginning of the LLVM entry block buffer, to
    // avoid ordering issues.
    if let Some(first_instruction) = llvm_entry_block.get_first_instruction() {
      llvm_builder.position_before(&first_instruction);
    } else {
      llvm_builder.position_at_end(llvm_entry_block);
    }

    llvm_builder.build_alloca(llvm_type.into(), name).unwrap()
  }

  /// Builds a comparison between the given subject and case values.
  /// The comparison will be for equality if the subject and case are both integers,
  /// or for approximate equality if the subject and case are both floating-point
  /// numbers.
  pub(crate) fn build_match_comparison(
    &mut self,
    is_int: bool,
    subject: inkwell::values::BasicValueEnum<'llvm>,
    case: inkwell::values::BasicValueEnum<'llvm>,
  ) -> inkwell::values::IntValue<'llvm> {
    if is_int {
      self
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::EQ,
          subject.into_int_value(),
          case.into_int_value(),
          "match.compare.int",
        )
        .unwrap()
    } else {
      self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OEQ,
          subject.into_float_value(),
          case.into_float_value(),
          "match.compare.float",
        )
        .unwrap()
    }
  }

  /// Lower a node with the `access_flag` flag set to the provided value.
  ///
  /// The previous `access_flag` flag will be restored once the node
  /// is lowered.
  pub(crate) fn lower_with_access_mode(
    &mut self,
    expr: &ast::Expr,
    access_mode: AccessMode,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // CONSIDER: Stripping all values from pointers when they're lowered with the `do_access` set to true. Actually this implicit behavior for POINTERS might cause issues, consider instead making a specialized function for this. This would act similar to Rust's implicit dereferencing of references, ex. `&&T -> T`.

    // NOTE: By using an inline buffer, this behaves as a stack.
    let previous_access_mode = self.access_mode;

    self.access_mode = access_mode;

    let llvm_value_opt = self.visit_expr(&expr);

    // Restore the buffer to its previous value after visiting the node.
    self.access_mode = previous_access_mode;

    llvm_value_opt
  }

  /// Access the given LLVM pointer value if the `access_flag` flag is raised.
  ///
  /// If the flag is not raised, no operation is performed, and the given value
  /// is returned unchanged.
  pub(crate) fn access_if_mode_applies(
    &mut self,
    llvm_pointee_type: inkwell::types::BasicTypeEnum<'llvm>,
    llvm_value: inkwell::values::PointerValue<'llvm>,
    debug_name: &str,
  ) -> inkwell::values::BasicValueEnum<'llvm> {
    // TODO: `AccessMode::Value` only applies to certain items, not all blindly! Rewrite this function to accept a type, and apply access mode depending on the given high-level type. This should also be used as the main access function.

    if let AccessMode::Value = self.access_mode {
      self.force_access(llvm_pointee_type, llvm_value, debug_name)
    } else {
      llvm_value.as_basic_value_enum()
    }
  }

  /// Create and return a snapshot copy of the current lowering buffers.
  pub(crate) fn save_state(&self) -> LoweringState<'llvm> {
    LoweringState {
      llvm_current_block: self.llvm_builder.get_insert_block(),
      llvm_current_function: self.llvm_function_buffer,
      llvm_entry_block: self.llvm_entry_block,
      access_mode: self.access_mode,
      // OPTIMIZE: Find a way to avoid cloning.
      runtime_guards_failure_buffers: self.runtime_guards_failure_buffers.clone(),
    }
  }

  /// Replace the local buffers with the given LLVM buffers object.
  ///
  /// The LLVM builder will be positioned at the end of the given block
  /// (if any).
  pub(crate) fn restore_state(&mut self, state: LoweringState<'llvm>) {
    self.llvm_function_buffer = state.llvm_current_function;

    if let Some(llvm_current_block) = state.llvm_current_block {
      self.llvm_builder.position_at_end(llvm_current_block);
    }

    self.access_mode = state.access_mode;
    self.llvm_entry_block = state.llvm_entry_block;
    self.runtime_guards_failure_buffers = state.runtime_guards_failure_buffers;
  }

  /// Mangle a name with a unique counter to avoid name collisions.
  pub(crate) fn mangle_name(&mut self, name: &str) -> String {
    format!(
      "{}_{}.{}",
      self.qualifier.package_name, self.qualifier.module_name, name
    )
  }

  /// Insert a `load` instruction for the given LLVM value.
  ///
  /// Equivalent to a de-reference of a pointer or copying. Does not make
  /// any exceptions or consider special cases.
  pub(crate) fn force_access(
    &self,
    llvm_pointee_type: inkwell::types::BasicTypeEnum<'llvm>,
    llvm_value: inkwell::values::PointerValue<'llvm>,
    debug_name: &str,
  ) -> inkwell::values::BasicValueEnum<'llvm> {
    self
      .llvm_builder
      .build_load(
        llvm_pointee_type,
        llvm_value,
        &format!("access.{}", debug_name),
      )
      .unwrap()
      .as_basic_value_enum()
  }

  pub(crate) fn lower_union_variant_type(
    &self,
    kind: &ast::UnionVariantKind,
  ) -> Option<inkwell::types::BasicTypeEnum<'llvm>> {
    match &kind {
      ast::UnionVariantKind::Type(tuple_type) => self.lower_type(&tuple_type),
      ast::UnionVariantKind::Singleton { .. } => Some(
        self
          .llvm_module
          .get_context()
          .i64_type()
          .as_basic_type_enum(),
      ),
      ast::UnionVariantKind::String(..) => Some(
        self
          .llvm_module
          .get_context()
          .i8_type()
          .ptr_type(inkwell::AddressSpace::default())
          .as_basic_type_enum(),
      ),
    }
  }

  fn lower_union_type(&self, union: &ast::Union) -> inkwell::types::BasicTypeEnum<'llvm> {
    let llvm_variants = union
      .variants
      .values()
      .into_iter()
      .map(|variant| self.lower_union_variant_type(&variant.kind))
      .collect::<Vec<_>>();

    let llvm_context = self.llvm_module.get_context();

    // OPTIMIZE: Can be optimized by having their tag ids be indexes. Then the type would be i32 (or less). Or can just be left like this? Type might still need to be adjusted.
    let llvm_tag_type = llvm_context.i64_type();

    // FIXME: The size should be based on the max/highest variant size.
    let size_type = llvm_context
      .i32_type()
      .array_type(Self::assert_trunc_cast(llvm_variants.len()));

    // TODO: Follow guide from: https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/basic-constructs/unions.html. Tagged unions.

    llvm_context
      .struct_type(&[llvm_tag_type.into(), size_type.into()], false)
      .as_basic_type_enum()
  }

  // CONSIDER: Separating strip logic for raw type into a `lower_raw_type` function, which would also make more semantic sense and no need to manually overcome the `.0` step of RawType, which kind of defeats its semantic meaning purpose.
  /// Lower the given type into its corresponding LLVM basic type.
  ///
  /// No caching or memoization will be performed.
  ///
  /// If the type being lowered is unit, `None` will be returned. All other
  /// types will return a `Some` value.
  pub(crate) fn lower_type(
    &self,
    ty: &types::Type,
  ) -> Option<inkwell::types::BasicTypeEnum<'llvm>> {
    // TODO: Accept a recursion limit parameter. Figure out how to fail in that case (cannot use panic). Would need to create a special failure/I.C.E. function in that case (that perhaps uses libc's abort).

    match self.resolve_type(ty).as_ref() {
      // REVIEW: Since now unit types DO lower to LLVM struct values, shouldn't it no longer return `Option`, but instead the actual lowered unit type, instead of it being a special case for parameters? This would be more consistent and avoid special treatment of parameters.
      types::Type::Unit => None,
      types::Type::Object(object_type) => Some(self.lower_object_type(object_type).as_basic_type_enum()),
      types::Type::Union(union) => Some(self.lower_union_type(union)),
      types::Type::Primitive(primitive_type) => Some(self.lower_primitive_type(primitive_type)),
      // SAFETY: Doesn't memory alignment have to be considered?
      // NOTE: Pointee type is irrelevant here; As of LLVM 15, all pointer types are opaque.
      types::Type::Opaque => Some(self.llvm_module.get_context().bool_type().ptr_type(inkwell::AddressSpace::default()).as_basic_type_enum()),
      // NOTE: References are simply pointers in the context of lowering.
      types::Type::Pointer(pointee_type) | types::Type::Reference(pointee_type) => self
        .lower_type(&pointee_type)
        .map(|ty| ty.ptr_type(inkwell::AddressSpace::default())
        .as_basic_type_enum()),
      // LLVM function types are not directly compatible with LLVM basic types.
      // This is because only functions themselves may hold function types. In
      // other words, no `alloca` can be made of type function. Instead, function
      // types are represented as pointers to function types elsewhere.
      types::Type::Signature(signature_type) => Some(
        // FIXME: Temporarily passing no closure captures. NEED to take this into account!
        self.lower_signature_type(signature_type, None)
          .ptr_type(inkwell::AddressSpace::default())
          .as_basic_type_enum()
      ),
      types::Type::Tuple(types::TupleType(element_types)) => {
        let llvm_field_types = element_types
          .iter()
          .map(|element_type| self.lower_type(element_type))
          // FIXME: Temporarily flattening without checks.
          .flatten()
          .collect::<Vec<_>>();

        Some(self.llvm_module.get_context().struct_type(&llvm_field_types, false).as_basic_type_enum())
      }
      types::Type::Stub(_) => unreachable!("stub type layers should have been stripped when the type being matched was resolved"),
      types::Type::Generic(..) => unreachable!("generic types should have been fully resolved"),
      types::Type::Range(..)
      // SAFETY: What about the case for hints? Is there use of hints over the type cache anywhere during lowering?
      | types::Type::Variable { .. } => unreachable!("meta types should be present after the type unification phase")
    }
  }

  fn lower_primitive_type(
    &self,
    primitive_type: &types::PrimitiveType,
  ) -> inkwell::types::BasicTypeEnum<'llvm> {
    let llvm_context = self.llvm_module.get_context();

    match primitive_type {
      types::PrimitiveType::Bool => llvm_context.bool_type().as_basic_type_enum(),
      types::PrimitiveType::Char => llvm_context.i8_type().as_basic_type_enum(),
      types::PrimitiveType::CString => llvm_context
        .i8_type()
        .ptr_type(inkwell::AddressSpace::default())
        .as_basic_type_enum(),
      types::PrimitiveType::Integer(width, _) => llvm_context
        .custom_width_int_type(match width {
          types::BitWidth::Width8 => 8,
          types::BitWidth::Width16 => 16,
          types::BitWidth::Width32 => 32,
          types::BitWidth::Width64 => 64,
          types::BitWidth::Width128 => 128,
        })
        .as_basic_type_enum(),
      types::PrimitiveType::Real(width) => match width {
        types::BitWidth::Width8 => {
          unreachable!("8-bit width floating-point numbers are purposely not supported by LLVM")
        }
        types::BitWidth::Width16 => llvm_context.f16_type(),
        types::BitWidth::Width32 => llvm_context.f32_type(),
        types::BitWidth::Width64 => llvm_context.f64_type(),
        types::BitWidth::Width128 => llvm_context.f128_type(),
      }
      .as_basic_type_enum(),
    }
  }

  fn lower_object_type(
    &self,
    object_type: &types::ObjectType,
  ) -> inkwell::types::StructType<'llvm> {
    assert!(
      matches!(object_type.kind, types::ObjectKind::Closed),
      "object types should always be closed during lowering phase"
    );

    let llvm_field_types = object_type
      .fields
      .iter()
      .map(|field| {
        self
          .lower_type(&field.1)
          .unwrap_or_else(|| self.make_llvm_unit_type().as_basic_type_enum())
      })
      .collect::<Vec<_>>();

    // TODO: Named global LLVM struct types are required for recursive types. Thus, consider defaulting to named global struct types. Currently, all struct types are inlined.

    self
      .llvm_module
      .get_context()
      .struct_type(llvm_field_types.as_slice(), false)
  }

  /// Lower the given type by its id.
  ///
  /// This will perform caching and memoization.
  ///
  /// If the type being lowered is unit, `None` will be returned. All other
  /// types will return a `Some` value.
  pub(crate) fn lower_type_by_id(
    &self,
    type_id: &symbol_table::TypeId,
  ) -> Option<inkwell::types::BasicTypeEnum<'llvm>> {
    self.lower_type(&self.resolve_type_by_id(type_id))
  }

  pub(crate) fn make_llvm_unit_type(&self) -> inkwell::types::PointerType<'llvm> {
    self
      .llvm_module
      .get_context()
      .struct_type(&[], false)
      .ptr_type(inkwell::AddressSpace::default())
  }

  pub(crate) fn make_llvm_unit_value(&self) -> inkwell::values::PointerValue<'llvm> {
    self.make_llvm_unit_type().const_null()
  }

  /// Lower the given signature type into its corresponding LLVM function type.
  ///
  /// This does not affect the current LLVM buffers.
  pub(crate) fn lower_signature_type(
    &self,
    signature_type: &types::SignatureType,
    closure_captures: Option<&Vec<ast::ClosureCapture>>,
  ) -> inkwell::types::FunctionType<'llvm> {
    let mut llvm_parameter_types = signature_type
      .parameter_types
      .iter()
      .map(|parameter_type| {
        self
          .lower_type(&parameter_type)
          .unwrap_or_else(|| self.make_llvm_unit_type().as_basic_type_enum())
          .into()
      })
      .collect::<Vec<_>>();

    // If the signature includes closure captures, the signature type must be modified
    // to include the captures environment parameter, in order to prevent LLVM
    // from having internal issues which lead to weird behavior that not even the
    // verifier is able to catch, and of course, for correctness.
    if let Some(closure_captures) = closure_captures {
      if !closure_captures.is_empty() {
        let captures_env_type = self
          .create_captures_env_type(&closure_captures)
          .into_pointer_type();

        let llvm_captures_env_type = self
          .lower_type(&captures_env_type)
          .expect(BUG_TYPE_NEVER_UNIT);

        llvm_parameter_types.push(llvm_captures_env_type.into());
      }
    }

    if let Some(llvm_return_type) = self.lower_type(&signature_type.return_type) {
      return llvm_return_type.fn_type(
        llvm_parameter_types.as_slice(),
        signature_type.arity_mode.is_variadic(),
      );
    }

    self.llvm_module.get_context().void_type().fn_type(
      llvm_parameter_types.as_slice(),
      signature_type.arity_mode.is_variadic(),
    )
  }

  /// Forcefully retrieve the current LLVM basic block.
  ///
  /// # Panics
  ///
  /// If the LLVM basic block buffer is unset, this operation will panic.
  pub(crate) fn get_current_block(&self) -> inkwell::basic_block::BasicBlock<'llvm> {
    self
      .llvm_builder
      .get_insert_block()
      .expect(auxiliary::BUG_BUFFER_CONTRACT)
  }

  /// Attempt to build a `ret` instruction on the current LLVM basic block
  /// buffer.
  ///
  /// If the basic block already contains a terminator instruction, nothing will
  /// occur. Otherwise, if the value is `None`, a `ret` instruction with no value
  /// (`ret void`) will be built.
  pub(crate) fn try_build_ret(
    &mut self,
    return_value: Option<inkwell::values::BasicValueEnum<'llvm>>,
  ) {
    // CONSIDER: Merging this with the function's terminator check, for void functions?
    // Only build a single return instruction per block.
    if self.get_current_block().get_terminator().is_some() {
      return;
    } else if let Some(return_value) = return_value {
      self
        .llvm_builder
        .build_return(Some(&return_value))
        .expect(lowering::BUG_BUILDER_UNSET);
    } else {
      self
        .llvm_builder
        .build_return(None)
        .expect(lowering::BUG_BUILDER_UNSET);
    }
  }

  /// Given a polymorphic function, this function will attempt to specialize
  /// the function, substituting the generic parameter types. The resulting
  /// function is a *monomorphism* (or *monomorphic function*), which is a
  /// function whose generic parameters have been instantiated with concrete
  /// types.
  ///
  /// If the given function is not polymorphic, or if there is a discrepancy
  /// in the amount of generic and specialized parameters, this function will
  /// return `None`, indicating that the function could not be specialized.
  ///
  /// If the same monomorphism has already been created, this function will
  /// return the memoized monomorphism. In other words, memoization is taken
  /// into account by this function.
  pub(crate) fn lower_artifact(
    &mut self,
    artifact_id: symbol_table::UniverseId,
    item: &ast::Item,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // CONSIDER: Adding support (via an optional parameter) for access mode. Only do this if it's actually used, otherwise keep it simple.

    let previous_universe_stack = self.universe_stack.clone();
    let mut temporary_universe_stack = self.universe_stack.clone();

    temporary_universe_stack.push(artifact_id);

    // CONSIDER: (poly-memoization) Move monomorphism memoization logic here? Or would that violate the separation of concerns principle?
    self.universe_stack = temporary_universe_stack;

    let llvm_value = self.visit_item(item);

    // Once the monomorphic substitution environment has been prepared, proceed to
    // lower the function. Any generic types encountered through the function's
    // descendant nodes will be substituted with the monomorphic types.
    self.universe_stack = previous_universe_stack;

    llvm_value
  }

  /// Memoize a global string pointer literal.
  ///
  /// If the given string literal has already been memoized, this function will
  /// return the memoized value. Otherwise, this function will create a new
  /// global string pointer literal and memoize it.
  ///
  /// This function is useful for avoiding duplicate global string pointer
  /// literals, which can be beneficial for reducing the size of the generated
  /// LLVM IR.
  pub(crate) fn intern_or_get_string(
    &mut self,
    string: &str,
    name: &str,
  ) -> inkwell::values::BasicValueEnum<'llvm> {
    if let Some(llvm_existing_value) = self.interned_string_literals.get(string) {
      return llvm_existing_value.to_owned();
    }

    let llvm_string_value = self
      .llvm_builder
      .build_global_string_ptr(string, name)
      .unwrap()
      .as_basic_value_enum();

    self
      .interned_string_literals
      .insert(string.to_owned(), llvm_string_value);

    llvm_string_value
  }

  pub(crate) fn build_elif_branch(
    &mut self,
    branch: &(ast::Expr, ast::Expr),
    index: usize,
    llvm_current_function: inkwell::values::FunctionValue<'llvm>,
    llvm_elif_guard_blocks: &[inkwell::basic_block::BasicBlock<'llvm>],
    llvm_detour_block: &inkwell::basic_block::BasicBlock<'llvm>,
    llvm_after_block: inkwell::basic_block::BasicBlock<'llvm>,
    llvm_yield_value_alloca_opt: &Option<inkwell::values::PointerValue<'llvm>>,
  ) {
    // First, lower the `then` block of the else-if branch to be executed as
    // the body of the else-if.
    let llvm_elif_then_block = self
      .llvm_module
      .get_context()
      .append_basic_block(llvm_current_function, "if.elif.then");

    // REVIEW: There might be a better way to go about this? Maybe we can simplify it in a way where we don't have to use indexing?
    // Then, choose the next bridge block or the else as the default target.
    // This block will contain the branching logic to fallthrough to the next
    // else if block, the else block (if any), or finally, the after block.
    let next_guard_or_detour = if let Some(guard_block) = llvm_elif_guard_blocks.get(index + 1) {
      guard_block
    } else {
      &llvm_detour_block
    };

    // Position at the current guard block to create the jump condition
    // and instruction.
    self
      .llvm_builder
      // REVISE: Use an assertion to explain why the index should exist. Since this is an extension of a high-level, not an utility function, it is acceptable and preferable for it to have assumptions (assertions), instead of something like a `Result`.
      .position_at_end(llvm_elif_guard_blocks[index]);

    // REVIEW: No need to specify access? I think there might be a need to specify the value to be accessed if possible/applicable.
    let llvm_condition = self
      .visit_expr(&branch.0)
      .expect(BUG_LLVM_VALUE)
      .into_int_value();

    // Link the current guard block to either the `then` block, or the
    // next guard block up on the chain.
    self
      .llvm_builder
      .build_conditional_branch(
        llvm_condition,
        llvm_elif_then_block,
        // NOTE: Only the underlying pointer is copied.
        next_guard_or_detour.to_owned(),
      )
      .expect(lowering::BUG_BUILDER_UNSET);

    // Emit the `then` block's contents.
    self.llvm_builder.position_at_end(llvm_elif_then_block);

    let llvm_else_if_value_opt = self.visit_expr(&branch.1);

    // Store the yield value, if applicable.
    if let Some(llvm_yield_value_alloca) = llvm_yield_value_alloca_opt {
      self
        .llvm_builder
        // SAFETY: Use `expect` here, and provide reasoning.
        // REVIEW: Why is `llvm_else_if_value_opt` `Option`al?
        .build_store(*llvm_yield_value_alloca, llvm_else_if_value_opt.unwrap())
        .expect(lowering::BUG_BUILDER_UNSET);
    }

    // Connect the `then` block to the `after` block. This serves as fallthrough.
    self
      .llvm_builder
      .build_unconditional_branch(llvm_after_block)
      .expect(lowering::BUG_BUILDER_UNSET);

    // Position the builder on the upcoming guard block or the detour block if
    // there aren't any more guard blocks.
    self
      .llvm_builder
      // NOTE: Only the underlying pointer is copied.
      .position_at_end(next_guard_or_detour.to_owned());
  }

  pub(crate) fn build_then_branch(
    &mut self,
    if_: &ast::If,
    llvm_current_function: inkwell::values::FunctionValue<'llvm>,
    llvm_yield_value_alloca_opt: Option<inkwell::values::PointerValue<'llvm>>,
    llvm_after_block: inkwell::basic_block::BasicBlock<'llvm>,
  ) -> inkwell::basic_block::BasicBlock<'llvm> {
    let llvm_then_block = self
      .llvm_module
      .get_context()
      .append_basic_block(llvm_current_function, "if.then");

    self.llvm_builder.position_at_end(llvm_then_block);

    // REVIEW: Shouldn't be passing `AccessMode::Value` here? Instead using `lower_value`?
    let llvm_then_block_value_opt = self.visit_expr(&if_.then_branch);

    if let Some(llvm_yield_value_alloca) = llvm_yield_value_alloca_opt {
      self
        .llvm_builder
        // TODO: Use `expect` here to describe why it is expected to be `Some`.
        .build_store(llvm_yield_value_alloca, llvm_then_block_value_opt.unwrap())
        .expect(lowering::BUG_BUILDER_UNSET);
    }

    // REVIEW: Since we removed return statements, isn't fallthrough always going to occur? If so, we don't need this terminator check?
    // REVIEW: Is this correct? Or should we be using `get_current_block()` here? Or maybe this is just a special case to not leave the `then` block without a terminator? Investigate.
    // Fallthrough the `then` block if applicable.
    if self.get_current_block().get_terminator().is_none() {
      self
        .llvm_builder
        .build_unconditional_branch(llvm_after_block)
        .expect(lowering::BUG_BUILDER_UNSET);
    }

    llvm_then_block
  }

  pub(crate) fn build_if_detour_block(
    &mut self,
    if_: &ast::If,
    llvm_current_function: inkwell::values::FunctionValue<'llvm>,
    llvm_yield_value_alloca_opt: &Option<inkwell::values::PointerValue<'llvm>>,
    llvm_after_block: inkwell::basic_block::BasicBlock<'llvm>,
    llvm_initial_block: inkwell::basic_block::BasicBlock<'llvm>,
  ) -> inkwell::basic_block::BasicBlock<'llvm> {
    let else_branch = match &if_.else_branch {
      Some(llvm_else_block) => llvm_else_block,
      None => return llvm_after_block,
    };

    let llvm_else_block = self
      .llvm_module
      .get_context()
      .append_basic_block(llvm_current_function, "if.else");

    self.llvm_builder.position_at_end(llvm_else_block);

    // REVIEW: Shouldn't we specify to lower with access mode set here?
    let llvm_else_branch_value = self.visit_expr(else_branch);

    if let Some(llvm_yield_value_alloca) = llvm_yield_value_alloca_opt {
      self
        .llvm_builder
        .build_store(
          *llvm_yield_value_alloca,
          // If the yield value allocation is present, then it means that
          // the if construct yields a non-unit value. Therefore, the else
          // branch must yield a value.
          llvm_else_branch_value.expect(BUG_LLVM_VALUE),
        )
        .expect(lowering::BUG_BUILDER_UNSET);
    }

    // REVIEW: Since we removed return statements, isn't fallthrough always going to occur? If so, we don't need this terminator check?
    // REVIEW: Is this correct? Or should we be using the `else_block` directly here?
    // Fallthrough from the else block to the after block if applicable.
    if self.get_current_block().get_terminator().is_none() {
      self
        .llvm_builder
        .build_unconditional_branch(llvm_after_block)
        .expect(lowering::BUG_BUILDER_UNSET);
    }

    self.llvm_builder.position_at_end(llvm_initial_block);

    llvm_else_block
  }

  fn attach_attributes(
    &self,
    llvm_function: &inkwell::values::FunctionValue<'llvm>,
    attributes: &[u32],
  ) {
    for attribute in attributes {
      let llvm_attribute = self
        .llvm_module
        .get_context()
        .create_enum_attribute(*attribute, 0);

      llvm_function.add_attribute(inkwell::attributes::AttributeLoc::Function, llvm_attribute);
    }
  }

  fn get_or_insert_libc_abort(&self) -> inkwell::values::FunctionValue<'llvm> {
    const LIBC_ABORT_FUNCTION_NAME: &str = "abort";

    // TODO: Need to verify that the existing definition matches expected signature. If not, that is allowed as well; a solution to user-land `abort` extern clashing with internal panicking `abort` is yet to be determined, but at least a panic should be issued.
    // TODO: One solution to the clashing problem would be that if it's already imported, use bitcast to shape the extern into the expected signature. This would be a bit of a hack, but it would work.
    if let Some(existing_definition) = self.llvm_module.get_function(LIBC_ABORT_FUNCTION_NAME) {
      return existing_definition;
    }

    let llvm_function_type = self
      .llvm_module
      .get_context()
      .void_type()
      .fn_type(&[], false);

    let llvm_abort_function = self.llvm_module.add_function(
      LIBC_ABORT_FUNCTION_NAME,
      llvm_function_type,
      Some(inkwell::module::Linkage::External),
    );

    let llvm_noreturn_attribute =
      inkwell::attributes::Attribute::get_named_enum_kind_id("noreturn");

    self.attach_attributes(&llvm_abort_function, &[llvm_noreturn_attribute]);

    llvm_abort_function
  }

  fn get_or_insert_libc_puts(&self) -> inkwell::values::FunctionValue<'llvm> {
    const LIBC_PUTS_FUNCTION_NAME: &str = "puts";

    if let Some(existing_definition) = self.llvm_module.get_function(LIBC_PUTS_FUNCTION_NAME) {
      return existing_definition;
    }

    let llvm_module = self.llvm_module.get_context();

    let llvm_function_type = llvm_module.i32_type().fn_type(
      &[llvm_module
        .i8_type()
        .ptr_type(inkwell::AddressSpace::default())
        .as_basic_type_enum()
        .into()],
      false,
    );

    self.llvm_module.add_function(
      LIBC_PUTS_FUNCTION_NAME,
      llvm_function_type,
      Some(inkwell::module::Linkage::External),
    )
  }

  /// Insert a runtime assertion that will abort the program and display an error
  /// message at runtime if the given condition is not met.
  ///
  /// This is useful for ensuring that the program does not enter an invalid state
  /// at runtime (undefined behavior).
  pub(crate) fn insert_runtime_guard(
    &mut self,
    llvm_condition: inkwell::values::IntValue<'llvm>,
    guard: RuntimeGuard,
  ) {
    let llvm_function = self
      .llvm_function_buffer
      .expect(auxiliary::BUG_BUFFER_CONTRACT);

    let llvm_context = self.llvm_module.get_context();

    let llvm_continuation_block =
      llvm_context.append_basic_block(llvm_function, "guard.continuation");

    if let Some(cached_runtime_guard_failure_block) =
      self.runtime_guards_failure_buffers.get(&guard)
    {
      self
        .llvm_builder
        .build_conditional_branch(
          llvm_condition,
          llvm_continuation_block,
          *cached_runtime_guard_failure_block,
        )
        .expect(lowering::BUG_BUILDER_UNSET);
    } else {
      let llvm_failure_block = llvm_context.append_basic_block(llvm_function, "guard.failure");

      self
        .llvm_builder
        .build_conditional_branch(llvm_condition, llvm_continuation_block, llvm_failure_block)
        .expect(lowering::BUG_BUILDER_UNSET);

      self
        .runtime_guards_failure_buffers
        .insert(guard, llvm_failure_block);

      self.llvm_builder.position_at_end(llvm_failure_block);

      let message = match guard {
        RuntimeGuard::DivisionByZero => "division by zero",
        RuntimeGuard::NullDereference => "dereference of a null pointer",
      };

      let llvm_message = self.intern_or_get_string(
        &format!("runtime assertion failed: {}", message),
        "guard.message",
      );

      let llvm_libc_puts = self.get_or_insert_libc_puts();

      self
        .llvm_builder
        .build_direct_call(
          // CONSIDER: Printing error message to `stderr` instead of `stdout` (which is what `puts` does by default). For that, it would be necessary to use a different libc function, such as `fputs`. It would also be necessary to add a `stderr` global variable to the LLVM module.
          llvm_libc_puts,
          &[llvm_message.into()],
          "guard.puts",
        )
        .expect(lowering::BUG_BUILDER_UNSET);

      let llvm_note_message = self.intern_or_get_string("the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior", "guard.note.message");

      self
        .llvm_builder
        .build_direct_call(
          llvm_libc_puts,
          &[llvm_note_message.into()],
          "guard.note.puts",
        )
        .expect(lowering::BUG_BUILDER_UNSET);

      self
        .llvm_builder
        .build_direct_call(self.get_or_insert_libc_abort(), &[], "guard.abort")
        .expect(lowering::BUG_BUILDER_UNSET);

      self
        .llvm_builder
        .build_unreachable()
        .expect(lowering::BUG_BUILDER_UNSET);
    };

    // Leave the LLVM builder positioned at the end of the continuation block, so that
    // the caller can continue lowering after the guard's logic was inserted.
    self.llvm_builder.position_at_end(llvm_continuation_block);
  }

  pub(crate) fn resolve_type<'b>(
    &'b self,
    ty: &'b types::Type,
  ) -> std::borrow::Cow<'b, types::Type> {
    self
      .resolution_helper
      .base
      .resolve(ty, self.universe_stack.to_owned())
      .expect(BUG_INSTANTIATION)
  }

  pub(crate) fn resolve_type_by_id<'b>(
    &'b self,
    type_id: &symbol_table::TypeId,
  ) -> std::borrow::Cow<'b, types::Type> {
    self
      .resolution_helper
      .resolve_by_id(type_id, self.universe_stack.to_owned())
      .expect(BUG_INSTANTIATION)
  }

  pub(crate) fn create_captures_env_type(&self, captures: &[ast::ClosureCapture]) -> types::Type {
    let mut closure_environment_fields = types::ObjectFieldMap::new();

    for capture in captures {
      closure_environment_fields.insert(
        capture.name.to_owned(),
        self.resolve_type_by_id(&capture.type_id).into_owned(),
      );
    }

    types::Type::Object(types::ObjectType {
      fields: closure_environment_fields,
      kind: types::ObjectKind::Closed,
    })
  }

  pub(crate) fn build_call_site_closure_env(
    &mut self,
    captures: &[ast::ClosureCapture],
  ) -> inkwell::values::PointerValue<'llvm> {
    let llvm_capture_env_type = self
      .lower_type(&self.create_captures_env_type(&captures))
      .expect(BUG_TYPE_NEVER_UNIT);

    let llvm_capture_env = self
      .llvm_builder
      .build_malloc(llvm_capture_env_type, "closure.captures")
      .expect("LLVM capture environment type should be sized");

    let llvm_zero = self.llvm_module.get_context().i32_type().const_zero();

    for capture in captures {
      let capture_registry_item = self
        .symbol_table
        .follow_link(&capture.target_link_id)
        .expect(auxiliary::BUG_NAME_RESOLUTION);

      let llvm_capture_target_opt = match capture_registry_item {
        symbol_table::RegistryItem::Parameter(parameter) => {
          self.visit_item(&ast::Item::Parameter(parameter.clone()))
        }
        symbol_table::RegistryItem::Binding(binding) => {
          let saved_access_mode = self.access_mode;

          self.access_mode = AccessMode::Value;

          let llvm_binding = self.visit_item(&ast::Item::Binding(binding.clone()));

          self.access_mode = saved_access_mode;

          llvm_binding
        }
        expr_registry_item => {
          let capture_expr = expr_registry_item
            .into_expr()
            .expect("capture target should be an expression by this point");

          self.lower_with_access_mode(&capture_expr, AccessMode::Value)
        }
      };

      // If the capture target yielded a unit value, then
      // use the LLVM unit value instead. Do not skip the
      // capture, as doing so might lead to logic bugs related
      // to gaps in the capture environment in terms of indexes.
      let llvm_capture_target = match llvm_capture_target_opt {
        Some(llvm_capture_target) => llvm_capture_target,
        None => self.make_llvm_unit_value().as_basic_value_enum(),
      };

      let llvm_index = self
        .llvm_module
        .get_context()
        .i32_type()
        .const_int(capture.index as u64, false);

      let llvm_capture_env_field = unsafe {
        self
          .llvm_builder
          .build_gep(
            llvm_capture_env_type,
            llvm_capture_env,
            &[llvm_zero, llvm_index],
            "closure.captures.populate.gep",
          )
          .unwrap()
      };

      self
        .llvm_builder
        .build_store(llvm_capture_env_field, llvm_capture_target)
        .expect(lowering::BUG_BUILDER_UNSET);
    }

    llvm_capture_env
  }
}
