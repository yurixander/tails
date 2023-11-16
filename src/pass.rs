//! Contains utilities for the orchestration and execution of passes.

use crate::{
  ast, auxiliary, declare, diagnostic, inference, instantiation, lifetime, link, lowering_ctx,
  resolution, semantics, symbol_table, unification,
  visit::{self, Visitable, Visitor},
};

macro_rules! require_dependency {
  ($dependency:expr) => {
    match $dependency {
      Some(value) => value,
      None => return PassResult::UnmetDependencies,
    }
  };
}

macro_rules! require_maybe_many {
  ($result:expr) => {
    match $result {
      Ok(value) => value,
      Err(diagnostics) => return PassResult::Err(diagnostics),
    }
  };
}

pub enum PassResult {
  Ok(Vec<diagnostic::Diagnostic>),
  Err(Vec<diagnostic::Diagnostic>),
  UnmetDependencies,
  LlvmIrOutput(String),
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
  /// An analysis pass upon the AST that may produce diagnostics.
  Analysis,
  /// A backend-focused, lowering pass that produces an output string in a lower-level IR or
  /// target language.
  ///
  /// Backend passes are always executed last.
  Backend,
}

#[derive(Clone, Copy)]
pub struct PassInfo {
  /// Used to uniquely identify a pass.
  id: PassId,
  /// The type or classification of the pass. This affects the ordering in which the pass
  /// is executed.
  kind: PassKind,
}

pub trait Pass {
  fn get_info(&self) -> PassInfo;

  fn run_on_module<'a>(
    &mut self,
    _module: &ast::Module,
    _context: &mut ExecutionContext<'a>,
  ) -> PassResult;

  fn run_on_context<'a>(&mut self, _context: &ExecutionContext<'a>) -> PassResult {
    PassResult::Ok(Vec::default())
  }
}

#[derive(Default)]
pub struct SemanticCheckPass;

impl Pass for SemanticCheckPass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::SemanticCheck,
      kind: PassKind::Analysis,
    }
  }

  fn run_on_module<'a>(
    &mut self,
    module: &ast::Module,
    context: &mut ExecutionContext<'a>,
  ) -> PassResult {
    let symbol_table = require_dependency!(&context.symbol_table);
    let type_env = require_dependency!(&context.type_env);
    let universes = require_dependency!(&context.universes);
    let resolution_helper = resolution::ResolutionHelper::new(universes, symbol_table, type_env);
    let reverse_universe_tracker = require_dependency!(&context.reverse_universe_tracker);

    let mut semantic_check_ctx =
      semantics::SemanticCheckContext::new(&symbol_table, &resolution_helper);

    // REVISE: Use the provided module instead. This will also get rid of the dependence on the module map from the context's fields.
    for global_item in &module.global_items {
      visit::traverse_possibly_polymorphic_global_item(
        global_item,
        reverse_universe_tracker,
        &mut semantic_check_ctx,
      );
    }

    diagnostic::DiagnosticsHelper::from(semantic_check_ctx.into_diagnostics()).into_pass_result()
  }
}

pub struct LoweringPass;

impl Pass for LoweringPass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::LlvmLowering,
      kind: PassKind::Backend,
    }
  }

  fn run_on_module<'a>(
    &mut self,
    module: &ast::Module,
    context: &mut ExecutionContext<'a>,
  ) -> PassResult {
    let symbol_table = require_dependency!(&context.symbol_table);
    let type_env = require_dependency!(&context.type_env);
    let universes = require_dependency!(&context.universes);
    let resolution_helper = resolution::ResolutionHelper::new(universes, symbol_table, type_env);
    let llvm_context = inkwell::context::Context::create();
    let llvm_module = llvm_context.create_module(&module.qualifier.to_string());

    let mut lowering_context = lowering_ctx::LoweringContext::new(
      module.qualifier.clone(),
      &symbol_table,
      &resolution_helper,
      &llvm_module,
    )
    .expect("a new, empty LLVM module should be valid");

    // FIXME: This doesn't apply to all cases; only used for tests.
    const ENTRY_POINT_NAME: &str = "tests";

    // OPTIMIZE: Better way to find the entry point function rather then just searching for a function named `main` through all modules O(m*n).
    let entry_point_opt = module.global_items.iter().find(|node| {
      if let ast::Item::Function(function) = node {
        function.name == ENTRY_POINT_NAME
      } else {
        false
      }
    });

    // TODO: If the package is not a library, then the entry point function should be required to exist.
    // Only visit the entry point function.
    if let Some(entry_point) = &entry_point_opt {
      lowering_context.visit_item(entry_point);
    } else {
      // FIXME: What if a module corresponds to a library or a package's module/file that has no entry point function? This needs to be updated to handle multiple modules per package.
      return PassResult::Err(vec![diagnostic::Diagnostic::MissingEntryPoint]);
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

    PassResult::LlvmIrOutput(llvm_module_string)
  }
}

#[derive(Default)]
pub struct LifetimeAnalysisPass;

impl Pass for LifetimeAnalysisPass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::LifetimeAnalysis,
      kind: PassKind::Analysis,
    }
  }

  fn run_on_module<'a>(
    &mut self,
    module: &ast::Module,
    context: &mut ExecutionContext<'a>,
  ) -> PassResult {
    let symbol_table = require_dependency!(&context.symbol_table);
    let type_env = require_dependency!(&context.type_env);
    let reverse_universe_tracker = require_dependency!(&context.reverse_universe_tracker);
    let universes = require_dependency!(&context.universes);
    let resolution_helper = resolution::ResolutionHelper::new(universes, symbol_table, type_env);

    let mut lifetime_analysis_ctx =
      lifetime::LifetimeAnalysisContext::new(symbol_table, &resolution_helper);

    lifetime_analysis_ctx.visit_module(module);

    for global_item in &module.global_items {
      visit::traverse_possibly_polymorphic_global_item(
        global_item,
        reverse_universe_tracker,
        &mut lifetime_analysis_ctx,
      );
    }

    diagnostic::DiagnosticsHelper::from(lifetime_analysis_ctx.diagnostics).into_pass_result()
  }
}

#[derive(Default)]
pub struct DeclarePass;

impl Pass for DeclarePass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::Resolution,
      kind: PassKind::Primary(0),
    }
  }

  fn run_on_module<'a>(
    &mut self,
    module: &ast::Module,
    context: &mut ExecutionContext<'a>,
  ) -> PassResult {
    assert!(
      !context.declarations.contains_key(&module.qualifier),
      "the same module should not be declared twice"
    );

    let mut declare_ctx = declare::DeclarationContext::default();

    for global_item in &module.global_items {
      global_item.traverse(&mut declare_ctx);
    }

    let diagnostic_helper = diagnostic::DiagnosticsHelper {
      diagnostics: declare_ctx.diagnostics,
    };

    if diagnostic_helper.contains_errors() {
      return PassResult::Err(diagnostic_helper.diagnostics);
    }

    context.symbol_table = Some(declare_ctx.symbol_table);

    context
      .declarations
      .insert(module.qualifier.clone(), declare_ctx.module_scope);

    PassResult::Ok(diagnostic_helper.diagnostics)
  }
}

#[derive(Default)]
pub struct LinkPass;

impl LinkPass {
  fn create_call_graph(symbol_table: &symbol_table::SymbolTable) -> auxiliary::CallGraph {
    let mut call_graph = auxiliary::CallGraph::default();

    for entry in symbol_table.registry.values() {
      if let symbol_table::RegistryItem::Function(function) = entry {
        call_graph.add_empty_entry(function.registry_id);
      } else if let symbol_table::RegistryItem::CallSite(call_site) = entry {
        let callee = match call_site.strip_callee(symbol_table) {
          Ok(callee) => callee,
          // NOTE: In case that the callee is not actually a callable expression,
          // simply ignore it and continue processing. This is a graceful
          // handling of the problem, as further phases will emit appropriate
          // diagnostics.
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

impl Pass for LinkPass {
  fn get_info(&self) -> PassInfo {
    PassInfo {
      id: PassId::Instantiation,
      kind: PassKind::Primary(1),
    }
  }

  fn run_on_module<'a>(
    &mut self,
    module: &ast::Module,
    context: &mut ExecutionContext<'a>,
  ) -> PassResult {
    let mut diagnostic_helper = diagnostic::DiagnosticsHelper::default();
    let symbol_table = require_dependency!(&mut context.symbol_table);

    // FIXME: The link pass still runs on all modules instead of per-module.
    for (qualifier, modules) in context.main_package {
      let mut link_ctx = link::LinkContext::new(&context.declarations, qualifier.clone())
        .expect("module should have been created on the declaration step");

      for global_item in &modules.global_items {
        global_item.traverse(&mut link_ctx);
      }

      diagnostic_helper.add_many(link_ctx.diagnostics);

      for (link_id, link) in link_ctx.links {
        assert!(
          !symbol_table.links.contains_key(&link_id),
          "links should not be declared twice"
        );

        symbol_table.links.insert(link_id, link);
      }
    }

    context.call_graph = Some(Self::create_call_graph(&symbol_table));

    diagnostic_helper.into_pass_result()
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
      // REVISE: Simplify nesting and usage of if/else if possible. Or, simply abstract to their own functions.
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
      kind: PassKind::Primary(2),
    }
  }

  fn run_on_module<'a>(
    &mut self,
    module: &ast::Module,
    context: &mut ExecutionContext<'a>,
  ) -> PassResult {
    let symbol_table = require_dependency!(&context.symbol_table);

    let mut inference_context =
      inference::InferenceContext::new(symbol_table, None, context.id_count);

    for global_item in &module.global_items {
      let is_polymorphic = global_item
        .find_generics()
        .map(|generics| !generics.parameters.is_empty())
        .unwrap_or(false);

      // Do not infer types for polymorphic items which aren't
      // invoked by artifacts.
      if !is_polymorphic {
        inference_context.visit(global_item);
      }
    }

    let instantiation_helper = instantiation::InstantiationHelper::new(symbol_table);
    let (universes, instantiation_diagnostics) = instantiation_helper.instantiate_all_artifacts();
    let diagnostics_helper = diagnostic::DiagnosticsHelper::from(instantiation_diagnostics);

    if diagnostics_helper.contains_errors() {
      return diagnostics_helper.into_pass_result();
    }

    assert!(
      universes.len() == symbol_table.artifacts.len(),
      "each artifact should have a corresponding universe"
    );

    let inference_results = inference_context.into_overall_result();

    let mut type_unification_context = unification::TypeUnificationContext::new(
      symbol_table,
      inference_results.type_var_substitutions,
      &universes,
    );

    let type_env = require_maybe_many!(type_unification_context
      .solve_constraints(&inference_results.type_env, &inference_results.constraints));

    let reverse_universe_tracker = Self::create_reverse_universe_tracker(&symbol_table);

    assert!(!diagnostics_helper.contains_errors());
    context.type_env = Some(type_env);
    context.id_count = inference_results.next_id_count;
    context.universes = Some(universes);
    context.reverse_universe_tracker = Some(reverse_universe_tracker);

    PassResult::Ok(diagnostics_helper.diagnostics)
  }
}

pub struct ExecutionContext<'a> {
  symbol_table: Option<symbol_table::SymbolTable>,
  call_graph: Option<auxiliary::CallGraph>,
  type_env: Option<symbol_table::TypeEnvironment>,
  universes: Option<instantiation::TypeSchemes>,
  reverse_universe_tracker: Option<instantiation::ReverseUniverseTracker>,
  id_count: usize,
  // FIXME: Should be removed and prefer only using a single module.
  main_package: &'a ast::Package,
  declarations: declare::DeclarationMap,
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
  pub results: PassResultsMap,
}

pub type PassResultsMap = std::collections::HashMap<PassId, PassResult>;

pub struct PassManager<'a> {
  passes: std::collections::BinaryHeap<PassEntry>,
  passes_tally: std::collections::HashSet<PassId>,
  main_package: &'a ast::Package,
}

impl<'a> PassManager<'a> {
  pub fn new(package: &'a ast::Package) -> Self {
    Self {
      passes: std::collections::BinaryHeap::new(),
      passes_tally: std::collections::HashSet::new(),
      main_package: package,
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

  pub fn add_primary_passes(&mut self) {
    self.add_default_pass::<DeclarePass>();
    self.add_default_pass::<LinkPass>();
    self.add_default_pass::<TypeInferencePass>();
    self.add_pass(Box::new(LoweringPass));
  }

  pub fn add_all_passes(&mut self) {
    self.add_primary_passes();
    self.add_default_pass::<SemanticCheckPass>();
    self.add_default_pass::<LifetimeAnalysisPass>();
  }

  // OPTIMIZE: There's a big optimization opportunity for visiting analysis passes that presents itself by using a pass manager. For all analysis passes, perform a single AST traversal, and abstract it here via the pass manager.
  pub fn run(&mut self, initial_id_count: usize) -> RunResult {
    let mut run_diagnostics = Vec::new();

    let mut context = ExecutionContext {
      symbol_table: None,
      call_graph: None,
      type_env: None,
      universes: None,
      reverse_universe_tracker: None,
      id_count: initial_id_count,
      main_package: self.main_package,
      declarations: declare::DeclarationMap::new(),
    };

    let mut results = PassResultsMap::new();

    while let Some(pass_entry) = self.passes.pop() {
      let mut pass = pass_entry.0;

      for module in self.main_package.values() {
        let info = pass.get_info();
        let run_result = pass.run_on_module(module, &mut context);

        // Extend diagnostics if the pass happened to produce any
        // diagnostics.
        match &run_result {
          PassResult::Err(diagnostics) => {
            run_diagnostics.extend(diagnostics.clone());

            // Cannot recover or continue if a pass failed.
            break;
          }
          PassResult::Ok(diagnostics) => {
            run_diagnostics.extend(diagnostics.clone());
          }
          _ => {}
        };

        results.insert(info.id, run_result);

        // CONSIDER: Having a trait `PassReportDiagnostic` implemented or as part of the `Pass` trait, that has a `.get_diagnostics()` function to avoid having to return diagnostics always as part of pass run results.
      }
    }

    // TODO: Handle and process dependencies.

    RunResult {
      diagnostics: run_diagnostics,
      results,
    }
  }
}
