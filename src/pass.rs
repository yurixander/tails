//! Contains utilities for the orchestration and execution of passes.

use crate::{
  ast, auxiliary, declare, diagnostic, inference, instantiation, lifetime, link, lowering_ctx,
  resolution, semantics, symbol_table, unification,
  visit::{self, Visitable, Visitor},
};

// CONSIDER: A verification pass, that will be used mostly for meta verification (debugging) purposes. This pass would check that all items have corresponding information registered on the symbol table, valid and expected types, etc. The checking phase could be mostly assertions. This can also be added from a driver to ensure more strict verification of the compiler's functionality.

macro_rules! require_dependency {
  ($dependency:expr) => {
    match &$dependency {
      Some(value) => value,
      None => return PassResult::UnmetDependencies,
    }
  };
}

macro_rules! require_maybe_many {
  ($result:expr) => {
    match $result {
      Ok(value) => value,
      Err(diagnostics) => return PassResult::Failed(diagnostics),
    }
  };
}

pub enum PassResultValue {
  Resolution {
    symbol_table: symbol_table::SymbolTable,
    call_graph: auxiliary::CallGraph,
  },
  TypeInference {
    type_env: symbol_table::TypeEnvironment,
    universes: instantiation::TypeSchemes,
    next_id_count: usize,
    reverse_universe_tracker: instantiation::ReverseUniverseTracker,
  },
  BackendOutput(String),
}

pub enum PassResult {
  Success(Vec<diagnostic::Diagnostic>),
  Failed(Vec<diagnostic::Diagnostic>),
  OkWithValue(PassResultValue, Vec<diagnostic::Diagnostic>),
  UnmetDependencies,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum PassId {
  LlvmLowering,
  Resolution,
  LifetimeAnalysis,
  SemanticCheck,
  TypeInference,
  Instantiation,
}

#[derive(PartialEq, Eq, PartialOrd, Clone, Copy)]
pub enum PassKind {
  /// Essential passes occur before other kinds of passes, because they provide result values
  /// that are needed as dependencies to other subsequent passes.
  Primary(usize),
  // CONSIDER: Passing analysis-only options (such as aggregate) when choosing analysis variant (turn it into a struct variant).
  /// An analysis pass upon the AST that may produce diagnostics.
  Analysis,
  /// A backend-focused, lowering pass that produces an output string in a lower-level IR or
  /// target language.
  ///
  /// Backend passes are always executed last.
  Backend,
}

// CONSIDER: Debugging/verification pass. Could be implemented as a simple traversal function call.

// CONSIDER: Providing a field that outlines dependencies via a vector of pass ids. This would get rid of the requirement of explicit ordering for essential passes.
#[derive(Clone, Copy)]
pub struct PassInfo {
  /// Used to uniquely identify a pass.
  id: PassId,
  /// The type or classification of the pass. This affects the ordering in which the pass
  /// is executed.
  kind: PassKind,
  /// Whether the pass supports parallelization.
  ///
  /// Not all passes that support parallelization will be *actually* parallelized. This
  /// is merely a hint when there is an opportunity for optimization, determined by the
  /// pass manager.
  supports_parallelization: bool,
}

pub trait Pass {
  fn get_info(&self) -> PassInfo;

  fn run<'a>(&mut self, _module: &ast::Module, _context: &ExecutionContext<'a>) -> PassResult;
}

#[derive(Default)]
pub struct SemanticCheckPass;

impl Pass for SemanticCheckPass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::SemanticCheck,
      kind: PassKind::Analysis,
      // TODO: For now, parallelization support is disabled for semantic check, until the implications are considered.
      supports_parallelization: false,
    }
  }

  fn run<'a>(&mut self, _module: &ast::Module, context: &ExecutionContext<'a>) -> PassResult {
    let symbol_table = require_dependency!(context.symbol_table);
    let type_env = require_dependency!(context.type_env);
    let universes = require_dependency!(context.universes);
    let resolution_helper = resolution::ResolutionHelper::new(universes, symbol_table, type_env);
    let reverse_universe_tracker = require_dependency!(context.reverse_universe_tracker);

    let mut semantic_check_ctx =
      semantics::SemanticCheckContext::new(&symbol_table, &resolution_helper);

    // REVISE: Use the provided unit node instead. This will also get rid of the dependence on the module map from the context's fields.
    for module in context.package.values() {
      for global_item in &module.global_items {
        visit::traverse_possibly_polymorphic_global_item(
          global_item,
          reverse_universe_tracker,
          &mut semantic_check_ctx,
        );
      }
    }

    diagnostic::DiagnosticsHelper::from(semantic_check_ctx.get_diagnostics())
      .into_analysis_pass_result()
  }
}

pub struct LoweringPass {
  module_qualifier: symbol_table::Qualifier,
}

impl Pass for LoweringPass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::LlvmLowering,
      kind: PassKind::Backend,
      supports_parallelization: false,
    }
  }

  fn run<'a>(&mut self, module: &ast::Module, context: &ExecutionContext<'a>) -> PassResult {
    let symbol_table = require_dependency!(context.symbol_table);
    let type_env = require_dependency!(context.type_env);
    let universes = require_dependency!(context.universes);
    let resolution_helper = resolution::ResolutionHelper::new(universes, symbol_table, type_env);
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module(&self.module_qualifier.module_name);

    let mut lowering_context = lowering_ctx::LoweringContext::new(
      self.module_qualifier.clone(),
      &symbol_table,
      &resolution_helper,
      &llvm_module,
    );

    const ENTRY_POINT_NAME: &str = "tests";

    // REVIEW: Is this necessary?
    let entry_point_opt = module.global_items.iter().find(|node| {
      if let ast::Item::Function(function) = node {
        // CONSIDER: All tests should move to simply having a main function.
        function.name == ENTRY_POINT_NAME
      } else {
        false
      }
    });

    // Visit the entry point function first, if it exists.
    if let Some(entry_point) = &entry_point_opt {
      lowering_context.visit_item(entry_point);
    } else {
      return PassResult::Failed(vec![diagnostic::Diagnostic::MissingEntryPoint]);
    }

    let llvm_module_string = llvm_module.print_to_string().to_string();

    // If an error occurred during the verification of the LLVM module, it means
    // that there is a logic bug present in the lowering code. In such a case, it
    // is acceptable to panic directly, as there is no point in propagating the error.
    if let Err(error_string) = llvm_module.verify() {
      panic!(
        "\n\n====== LLVM MODULE VERIFICATION FAILED ======\n{}\n====== LLVM MODULE STRING ======\n{}\n====== END ======\n",
        error_string.to_string(),
        llvm_module_string
      );
    }

    PassResult::OkWithValue(
      PassResultValue::BackendOutput(llvm_module_string),
      Vec::default(),
    )
  }
}

#[derive(Default)]
pub struct LifetimeAnalysisPass;

// CONSIDER: Moving the pass implementations to their corresponding modules.
impl Pass for LifetimeAnalysisPass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::LifetimeAnalysis,
      kind: PassKind::Analysis,
      supports_parallelization: true,
    }
  }

  fn run<'a>(&mut self, module: &ast::Module, context: &ExecutionContext<'a>) -> PassResult {
    let symbol_table = require_dependency!(context.symbol_table);
    let type_env = require_dependency!(context.type_env);
    let reverse_universe_tracker = require_dependency!(context.reverse_universe_tracker);
    let universes = require_dependency!(context.universes);
    let resolution_helper = resolution::ResolutionHelper::new(universes, symbol_table, type_env);

    let mut lifetime_analysis_ctx =
      lifetime::LifetimeAnalysisContext::new(symbol_table, &resolution_helper);

    lifetime_analysis_ctx.visit_module(module);

    for item in &module.global_items {
      visit::traverse_possibly_polymorphic_global_item(
        item,
        reverse_universe_tracker,
        &mut lifetime_analysis_ctx,
      );
    }

    diagnostic::DiagnosticsHelper::from(lifetime_analysis_ctx.diagnostics)
      .into_analysis_pass_result()
  }
}

#[derive(Default)]
pub struct ResolutionPass;

impl ResolutionPass {
  fn create_call_graph(symbol_table: &symbol_table::SymbolTable) -> auxiliary::CallGraph {
    let mut call_graph = auxiliary::CallGraph::default();

    for entry in symbol_table.registry.values() {
      if let symbol_table::RegistryItem::Function(function) = entry {
        call_graph.add_empty_entry(function.registry_id);
      } else if let symbol_table::RegistryItem::CallSite(call_site) = entry {
        // BUG: (test:resolution_missing_function) This needs to be done after type checking and semantic analysis, otherwise stripping callee may fail due to the fact that the assumptions don't hold true before type checking and possibly semantic analysis. There's another problem, however: If this is done after type checking, since type checking phase and instantiation phase is not yet equipped to handle recursive calls, it would go into a stack overflow for recursive calls. Need to figure out how to properly position+handle the creation of the call graph. For now, graceful handling is implemented to simply ignore and skip if the callee cannot be resolved, or is not actually callable.
        let callee = match call_site.strip_callee(symbol_table) {
          Ok(callee) => callee,
          Err(_) => continue,
        };

        let call_site_origin = symbol_table.call_site_parent_functions
          .get(&call_site.universe_id)
          .expect("all call sites should have a corresponding parent function declaration id on the symbol table");

        if let ast::Callable::Function(callee_function) = callee {
          call_graph.add_link(*call_site_origin, callee_function.registry_id);
        }
      }
    }

    let strongly_connected_components = call_graph.find_strongly_connected_components();

    // TODO: Handle direct & mutual recursion (not here; this is merely an assertion to ensure that mutually recursive inputs aren't processed).
    if strongly_connected_components
      .iter()
      .any(|set| set.len() > 1)
    {
      todo!("direct nor mutual recursion is not yet supported");
    }

    assert!(
      call_graph.check_whether_adjacency_list_is_closed(),
      "call graph should be closed; functions without outgoing edges should have an empty edge list"
    );

    call_graph
  }
}

impl Pass for ResolutionPass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::Resolution,
      kind: PassKind::Primary(0),
      supports_parallelization: false,
    }
  }

  fn run<'a>(&mut self, _module: &ast::Module, context: &ExecutionContext<'a>) -> PassResult {
    let first_key = match context.package.keys().next() {
      Some(key) => key.to_owned(),
      None => {
        return PassResult::OkWithValue(
          PassResultValue::Resolution {
            symbol_table: symbol_table::SymbolTable::default(),
            call_graph: auxiliary::CallGraph::default(),
          },
          Vec::default(),
        )
      }
    };

    let mut declare_ctx = declare::DeclarationContext::new(first_key);

    // REVIEW: Why not use the provided `Module` AST item?
    for (qualifier, module) in context.package {
      // REVIEW: Shouldn't the module be created before, on the Driver?
      declare_ctx.create_and_set_module(qualifier.clone());

      for item in &module.global_items {
        item.traverse(&mut declare_ctx);
      }
    }

    let mut diagnostics = diagnostic::DiagnosticsHelper {
      diagnostics: declare_ctx.diagnostics,
    };

    if diagnostics.contains_errors() {
      return PassResult::Failed(diagnostics.diagnostics);
    }

    let declarations = declare_ctx.declarations;
    let mut symbol_table = declare_ctx.symbol_table;

    for (qualifier, modules) in context.package {
      let mut link_ctx = link::LinkContext::new(&declarations, qualifier.clone())
        .expect("module should have been created on the declaration step");

      for item in &modules.global_items {
        item.traverse(&mut link_ctx);
      }

      diagnostics.add_many(link_ctx.diagnostics);
      symbol_table.links.extend(link_ctx.links);
    }

    let call_graph = Self::create_call_graph(&symbol_table);

    diagnostics.try_return_pass_result(PassResultValue::Resolution {
      symbol_table,
      call_graph,
    })
  }
}

#[derive(Default)]
pub struct TypeInferencePass;

impl TypeInferencePass {
  fn create_reverse_universe_tracker(
    symbol_table: &symbol_table::SymbolTable,
  ) -> instantiation::ReverseUniverseTracker {
    let mut reverse_universe_tracker = instantiation::ReverseUniverseTracker::new();

    for (artifact_id, artifact) in &symbol_table.artifacts {
      // REVISE: Simplify nesting and usage of if/else if possible.
      let registry_id = if let instantiation::Artifact::CallSite(call_site) = artifact {
        // BUG: This needs to be done after type checking and semantic analysis, otherwise stripping callee may fail due to the fact that the assumptions don't hold true before type checking and possibly semantic analysis. There's another problem, however: If this is done after type checking, since type checking phase and instantiation phase is not yet equipped to handle recursive calls, it would go into a stack overflow for recursive calls. Need to figure out how to properly position+handle the creation of the call graph.
        let callee = call_site.strip_callee(symbol_table).unwrap();

        let is_polymorphic = if let ast::Callable::Function(function) = &callee {
          function.is_polymorphic()
        } else {
          continue;
        };

        if !is_polymorphic {
          continue;
        }

        callee.get_registry_id()
      } else if let instantiation::Artifact::StubType(stub_type) = &artifact {
        let target = symbol_table
          .follow_link(&stub_type.path.link_id)
          .expect(auxiliary::BUG_NAME_RESOLUTION);

        let target_item = target.into_item();

        if let Some(target_item) = target_item {
          // Ignore non-polymorphic targets.
          if !target_item.is_polymorphic() {
            continue;
          }

          target_item
            .find_registry_id()
            .expect("polymorphic target items should have a declaration id property")
            .to_owned()
        } else {
          continue;
        }
      }
      // TODO: In the future, other possibly polymorphic targets should also be handled here (ex. unions).
      else {
        continue;
      };

      // BUG: (test:generics_call_chain) The problem seems to be the following:
      // 1. Reverse universe tracker processes all artifacts.
      // 2. It processes polymorphic/artifact call sites.
      // 3. It adds that call site's artifact id onto the reverse universe tracker.
      // 4. Polymorphic item traversal occurs with the reverse universe tracker (`visit::traverse_possibly_polymorphic_item`).
      // 5. The call site's artifact id is pushed onto the pass' universe stack via the `ArtifactContextSwitch` trait.
      // 6. Fault: The problem is that simply adding that call site's artifact id might not be enough: For example, if a function is called from two layers deep in terms of generics, the universe stack also needs the artifact ids of the layered calls, otherwise it would only add say X layer's artifact id, which itself has a generic type as part of its generic hints, thus leaving such generic type unable to be resolved because ITS call site's artifact id is not present!

      reverse_universe_tracker
        .entry(registry_id)
        .and_modify(|context_artifact_ids| context_artifact_ids.push(artifact_id.to_owned()))
        .or_insert(vec![artifact_id.to_owned()]);
    }

    reverse_universe_tracker
  }
}

impl Pass for TypeInferencePass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::TypeInference,
      kind: PassKind::Primary(1),
      supports_parallelization: false,
    }
  }

  fn run<'a>(&mut self, module: &ast::Module, context: &ExecutionContext<'a>) -> PassResult {
    let symbol_table = require_dependency!(context.symbol_table);

    let mut initial_inference_context =
      inference::InferenceContext::new(symbol_table, None, context.id_count);

    for item in &module.global_items {
      let is_polymorphic = item
        .find_generics()
        .map(|generics| !generics.parameters.is_empty())
        .unwrap_or(false);

      // Do not infer types for polymorphic items which aren't
      // invoked by artifacts.
      if !is_polymorphic {
        initial_inference_context.visit(item);
      }
    }

    let instantiation_helper = instantiation::InstantiationHelper::new(symbol_table);
    let (universes, instantiation_diagnostics) = instantiation_helper.instantiate_all_artifacts();
    let diagnostics_helper = diagnostic::DiagnosticsHelper::from(instantiation_diagnostics);

    if diagnostics_helper.contains_errors() {
      return diagnostics_helper.into_analysis_pass_result();
    }

    assert!(
      universes.len() == symbol_table.artifacts.len(),
      "each artifact should have a corresponding universe"
    );

    let inference_results = initial_inference_context.into_overall_result();

    let mut type_unification_context = unification::TypeUnificationContext::new(
      symbol_table,
      inference_results.type_var_substitutions,
      &universes,
    );

    let type_env = require_maybe_many!(type_unification_context
      .solve_constraints(&inference_results.type_env, &inference_results.constraints));

    let reverse_universe_tracker = Self::create_reverse_universe_tracker(&symbol_table);

    diagnostics_helper.try_return_pass_result(PassResultValue::TypeInference {
      type_env,
      next_id_count: inference_results.next_id_count,
      universes,
      reverse_universe_tracker,
    })
  }
}

pub struct ExecutionContext<'a> {
  pub backend_output: Option<String>,
  symbol_table: Option<symbol_table::SymbolTable>,
  call_graph: Option<auxiliary::CallGraph>,
  type_env: Option<symbol_table::TypeEnvironment>,
  universes: Option<instantiation::TypeSchemes>,
  reverse_universe_tracker: Option<instantiation::ReverseUniverseTracker>,
  id_count: usize,
  package: &'a ast::Package,
}

struct PassEntry(pub Box<dyn Pass>);

impl Eq for PassEntry {
  //
}

impl PartialEq for PassEntry {
  fn eq(&self, other: &Self) -> bool {
    self.0.get_info().kind == other.0.get_info().kind
  }
}

impl PartialOrd for PassEntry {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    // FIXME: Avoid repeating code. One `Ord` trait might need to be derived instead.
    Some(match (self.0.get_info().kind, other.0.get_info().kind) {
      (PassKind::Primary(priority_a), PassKind::Primary(priority_b)) => priority_b.cmp(&priority_a),
      (PassKind::Primary(_), _) => std::cmp::Ordering::Greater,
      (_, PassKind::Primary(_)) => std::cmp::Ordering::Less,
      // Lowering pass(es) should occur at the end.
      (PassKind::Backend, _) => std::cmp::Ordering::Less,
      (_, PassKind::Backend) => std::cmp::Ordering::Greater,
      _ => std::cmp::Ordering::Equal,
    })
  }
}

impl Ord for PassEntry {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    other
      .0
      .get_info()
      .kind
      .partial_cmp(&other.0.get_info().kind)
      // FIXME: Undocumented unwrap.
      .unwrap()
  }
}

pub struct RunResult {
  pub diagnostics: Vec<diagnostic::Diagnostic>,
  pub backend_output: Option<String>,
}

pub struct ExecutionOptions {
  // REVIEW: Would there ever be a case where parallelization is not desired? Perhaps during debugging since using parallelization would make it have non-deterministic behavior? If so, document why here, using a note.
  use_parallelization: bool,
}

impl Default for ExecutionOptions {
  fn default() -> Self {
    Self {
      use_parallelization: false,
    }
  }
}

// CONSIDER: Pass caching mechanism.
// CONSIDER: Pass metrics and benchmarking instrumentation.
pub struct PassManager<'a> {
  passes: std::collections::BinaryHeap<PassEntry>,
  passes_tally: std::collections::HashSet<PassId>,
  package: &'a ast::Package,
}

impl<'a> PassManager<'a> {
  fn apply_pass_result_value(value: PassResultValue, context: &mut ExecutionContext<'a>) {
    match value {
      PassResultValue::BackendOutput(output) => context.backend_output = Some(output),
      PassResultValue::Resolution {
        symbol_table,
        call_graph,
      } => {
        context.symbol_table = Some(symbol_table);
        context.call_graph = Some(call_graph);
      }
      PassResultValue::TypeInference {
        type_env,
        next_id_count,
        universes,
        reverse_universe_tracker,
      } => {
        context.universes = Some(universes);
        context.type_env = Some(type_env);
        context.id_count = next_id_count;
        context.reverse_universe_tracker = Some(reverse_universe_tracker);
      }
    };
  }

  pub fn new(package: &'a ast::Package) -> Self {
    Self {
      passes: std::collections::BinaryHeap::new(),
      passes_tally: std::collections::HashSet::new(),
      package,
    }
  }

  pub fn clear(&mut self) {
    self.passes.clear();
    self.passes_tally.clear();
  }

  pub fn has_pass(&self, id: &PassId) -> bool {
    self.passes_tally.contains(id)
  }

  pub fn add_pass(&mut self, pass: Box<dyn Pass>) -> bool {
    if !self.has_pass(&pass.get_info().id) {
      self.passes.push(PassEntry(pass));

      true
    } else {
      false
    }
  }

  pub fn add_default_pass<T: Pass + Default + 'static>(&mut self) -> bool {
    self.add_pass(Box::new(T::default()))
  }

  pub fn add_primary_passes(&mut self, module_qualifier: symbol_table::Qualifier) {
    self.add_default_pass::<ResolutionPass>();
    self.add_default_pass::<TypeInferencePass>();
    self.add_pass(Box::new(LoweringPass { module_qualifier }));
  }

  pub fn add_all_passes(&mut self, module_qualifier: symbol_table::Qualifier) {
    self.add_primary_passes(module_qualifier);
    self.add_default_pass::<SemanticCheckPass>();
    self.add_default_pass::<LifetimeAnalysisPass>();
  }

  // OPTIMIZE: There's a big optimization opportunity for visiting analysis passes that presents itself by using a pass manager. For all analysis passes, perform a single AST traversal, and abstract it here via the pass manager.
  pub fn run_with_options(
    &mut self,
    initial_id_count: usize,
    options: ExecutionOptions,
  ) -> RunResult {
    // TODO: Implement this functionality.
    if options.use_parallelization {
      todo!("using parallelization is not yet supported");
    }

    let mut run_diagnostics = Vec::new();

    let mut context = ExecutionContext {
      backend_output: None,
      symbol_table: None,
      call_graph: None,
      type_env: None,
      universes: None,
      reverse_universe_tracker: None,
      id_count: initial_id_count,
      package: self.package,
    };

    for module in self.package.values() {
      while let Some(pass_entry) = self.passes.pop() {
        let mut pass = pass_entry.0;

        // TODO: Make use of.
        let info = pass.get_info();

        let run_result = pass.run(module, &context);

        // CONSIDER: Having a trait `PassReportDiagnostic` implemented or as part of the `Pass` trait, that has a `.get_diagnostics()` function to avoid having to return diagnostics always as part of pass run results.

        match run_result {
          PassResult::Failed(diagnostics) => {
            run_diagnostics.extend(diagnostics);

            // Cannot recover or continue if a pass failed.
            break;
          }
          PassResult::Success(diagnostics) => {
            run_diagnostics.extend(diagnostics);
          }
          PassResult::UnmetDependencies => {
            // TODO: Report that the pass could not be completed.
            todo!("a pass had unmet dependencies: handling of this is not yet implemented");
          }
          PassResult::OkWithValue(value, diagnostics) => {
            run_diagnostics.extend(diagnostics);

            // CONSIDER: Ensuring that it was not already previously set to `Some` when changing context properties? Or is this a valid situation?
            // CONSIDER: Ensuring that the same pass doesn't run more than once? Or is it a valid situation?
            Self::apply_pass_result_value(value, &mut context);
          }
        }
      }
    }

    RunResult {
      diagnostics: run_diagnostics,
      backend_output: context.backend_output,
    }
  }

  pub fn run(&mut self, initial_id_count: usize) -> RunResult {
    self.run_with_options(initial_id_count, ExecutionOptions::default())
  }
}
