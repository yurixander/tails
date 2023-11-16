//! Responsible for instantiating generic and polymorphic types, and associating
//! their artifacts with the corresponding substitution tables "universes".
//!
//! In other words, this module is responsible for the "instantiation" phase of the compiler,
//! which mainly consists of the creation of "universes", which are then used on later
//! phases of the compiler to resolve generics, polymorphic types, and other artifacts.

use crate::{ast, auxiliary, diagnostic, inference, resolution, symbol_table, types, unification};

pub(crate) type ReverseUniverseTracker =
  std::collections::HashMap<symbol_table::RegistryId, Vec<symbol_table::UniverseId>>;

/// Contains substitution environments for generic types.
pub(crate) type TypeSchemes =
  std::collections::HashMap<symbol_table::UniverseId, symbol_table::SubstitutionEnv>;

#[derive(Debug, Clone)]
pub enum Artifact {
  CallSite(std::rc::Rc<ast::CallSite>),
  StubType(types::StubType),
}

pub(crate) struct InstantiationHelper<'a> {
  pub universes: TypeSchemes,
  symbol_table: &'a symbol_table::SymbolTable,
}

impl<'a> InstantiationHelper<'a> {
  /// Match monomorphic hints with a generics object, essentially instantiating
  /// it by creating a substitution environment in which the generics (for example,
  /// generic parameters) can be substituted with their monomorphic counterparts.
  ///
  /// This will fail if there is a mismatch in hints vs. generic parameter count.
  ///
  /// No additional validation is performed by this function (such as the unification of
  /// generic hints vs. parameters).
  pub(crate) fn substitute_generics_for_hints(
    &self,
    hints: &[types::Type],
    generics: &ast::Generics,
  ) -> diagnostic::Maybe<symbol_table::SubstitutionEnv> {
    if generics.parameters.len() != hints.len() {
      return Err(vec![
        diagnostic::Diagnostic::GenericParameterCountMismatch {
          expected: generics.parameters.len(),
          actual: hints.len(),
        },
      ]);
    }

    for hint in hints {
      let stripped_hint = hint
        .clone()
        // FIXME: Properly handle type strip error.
        .try_strip_all_monomorphic_stub_layers(self.symbol_table)
        .unwrap();

      let is_or_contains_generic_type = stripped_hint.is_a_generic()
        || hint
          .get_indirect_subtree_iter(self.symbol_table)
          // FIXME: Properly handle type strip error.
          .any(|ty| ty.unwrap().is_a_generic());

      // TODO: Handle the case where one of the provided hints is itself a generic, or contains a generic. Actually, a lot of tests that are using this concept seem to be passing. Consider it as a passing case, and write a note here for it?
      if is_or_contains_generic_type {
        // TRACE: (test:generics_call_chain) The test `generics_call_multi_artifacts` passes, even tho it hits this same condition (confirmed by commenting/uncommenting this `todo!()`). Could it be that both tests `generics_call_chain` and `generics_call_multi_artifacts` are somewhat different in terms of instantiation or inference?
        // todo!();
      }
    }

    let mut universe = symbol_table::SubstitutionEnv::new();

    for (hint, generic_parameter) in hints.iter().zip(generics.parameters.iter()) {
      assert!(
        !universe.contains_key(&generic_parameter.substitution_id),
        "the same generic parameter should not be substituted twice or more times"
      );

      universe.insert(generic_parameter.substitution_id, hint.to_owned());
    }

    assert!(universe.len() == hints.len());

    Ok(universe)
  }

  /// Unify two types for equality to determine whether they are
  /// equal.
  pub fn compare_by_unification(
    type_a: types::Type,
    type_b: types::Type,
    symbol_table: &symbol_table::SymbolTable,
  ) -> bool {
    // Both input types should be fully monomorphic, otherwise
    // instantiation would be needed to unify them properly.
    if type_a.is_a_generic()
      || type_b.is_a_generic()
      // FIXME: Properly handle results.
      || type_a.contains_generic_types(symbol_table).unwrap()
      || type_b.contains_generic_types(symbol_table).unwrap()
    {
      return false;
    }

    let universes = TypeSchemes::new();

    let mut type_unification_context = unification::TypeUnificationContext::new(
      symbol_table,
      symbol_table::SubstitutionEnv::new(),
      &universes,
    );

    let constraints = vec![inference::Constraint::Equality(type_a, type_b)]
      .into_iter()
      .map(|constraint| (resolution::UniverseStack::new(), constraint))
      .collect();

    type_unification_context
      .solve_constraints(&symbol_table::TypeEnvironment::new(), &constraints)
      .is_ok()
  }

  pub(crate) fn new(symbol_table: &'a symbol_table::SymbolTable) -> Self {
    Self {
      universes: TypeSchemes::new(),
      symbol_table,
    }
  }

  /// The entry point of the instantiation process.
  pub(crate) fn instantiate_all_artifacts(mut self) -> (TypeSchemes, Vec<diagnostic::Diagnostic>) {
    let mut diagnostics_helper = diagnostic::DiagnosticsHelper::default();

    for artifact in self.symbol_table.artifacts.values() {
      diagnostics_helper.add_many(match artifact {
        Artifact::StubType(stub_type) => self.instantiate_stub_type_artifact(stub_type),
        Artifact::CallSite(call_site) => self.instantiate_call_site(call_site),
      })
    }

    (self.universes, diagnostics_helper.diagnostics)
  }

  fn create_universe_for(
    &mut self,
    artifact_id: symbol_table::UniverseId,
    hints: &[types::Type],
    generics: &ast::Generics,
  ) -> Vec<diagnostic::Diagnostic> {
    // Delegate the creation of the substitution environment for
    // the polymorphic function's generics to the corresponding function,
    // that way the job of this function is simplified to just validation.
    let new_universe_result = self.substitute_generics_for_hints(&hints, &generics);

    // If the universe could not be created, then unification validation
    // cannot be performed; collect all diagnostics and return.
    let universe = match new_universe_result {
      Ok(universe) => universe,
      Err(diagnostics) => return diagnostics,
    };

    assert!(!self.universes.contains_key(&artifact_id));

    // CONSIDER: What about making the key of the universes be a universe id path? (Ie. call site universe id -> stub type universe id, etc.)? This way, 'nested' universes and the concept of 'nesting' is considered directly? This would also allow the retrieval of a target universe by a direct path (`O(1)`), instead of the current iterate-and-test approach (`O(n)`).
    self.universes.insert(artifact_id, universe);

    // NOTE: The hints do not need to be unified against the generic parameters,
    // as that would be redundant. This is because the generic parameters would
    // resolve to the hints themselves, therefore it would be like unifying the
    // hints against themselves.

    Vec::default()
  }

  fn instantiate_stub_type_artifact(
    &mut self,
    stub_type: &types::StubType,
  ) -> Vec<diagnostic::Diagnostic> {
    // NOTE: Type stubs are registered as artifacts during the declare
    // step only if they're polymorphic (ie. have generic hints), so
    // stripping monomorphic stub type here would be redundant.

    assert!(
      !stub_type.generic_hints.is_empty(),
      "stub type should have generic hints, and thus be polymorphic, otherwise it should not be registered as an artifact"
    );

    // REVIEW: What if the target is an artifact that accepts generics, but none were provided? Should that be reported here?
    assert!(
      !stub_type.generic_hints.is_empty(),
      "the stub type should have generic hints, and thus be polymorphic"
    );

    let target = self
      .symbol_table
      .follow_link(&stub_type.path.link_id)
      .expect(auxiliary::BUG_NAME_RESOLUTION);

    let target_type_def = match target {
      symbol_table::RegistryItem::TypeDef(type_def) => type_def,
      // TODO: Handle other targets.
      _ => todo!(),
    };

    // TODO: When unions are handled, this will need to be changed to a match case to extract the generics object. This way, the logic is more generalized to the generics object, and not just type defs.
    self.create_universe_for(
      stub_type.universe_id.to_owned(),
      &stub_type.generic_hints,
      &target_type_def.generics,
    )
  }

  fn instantiate_call_site(&mut self, call_site: &ast::CallSite) -> Vec<diagnostic::Diagnostic> {
    assert!(
      !call_site.generic_hints.is_empty(),
      "call site artifact should have generic hints, and thus be polymorphic, otherwise it should not be registered as an artifact"
    );

    let callee = call_site.strip_callee(self.symbol_table).unwrap();

    // Only functions can be polymorphic.
    let callee_function = match callee {
      ast::Callable::Function(callee_function) => callee_function,
      callable => {
        let name = callable
          .find_display_name()
          .expect("all callables should have a display name");

        return vec![diagnostic::Diagnostic::CalleeCannotAcceptGenericHints(name)];
      }
    };

    self.create_universe_for(
      call_site.universe_id.to_owned(),
      &call_site.generic_hints,
      &callee_function.generics,
    )
  }
}
