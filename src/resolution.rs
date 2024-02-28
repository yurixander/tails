use crate::{
  instantiation, symbol_table,
  types::{self, TypeStripError},
};

pub(crate) type UniverseStack = Vec<symbol_table::UniverseId>;

#[derive(Debug)]
pub(crate) enum TypeResolutionError {
  StubTypeMissingSymbolTableEntry,
  EmptyUniverseStackWhenResolvingGeneric,
  CouldNotFindSubstitutionInAnyUniverseInUniverseStack,
  NoUniversesWhenResolvingGeneric,
}

impl From<types::DirectRecursionCheckError> for TypeResolutionError {
  fn from(error: types::DirectRecursionCheckError) -> Self {
    match error {
      types::DirectRecursionCheckError::SymbolTableMissingEntry => {
        TypeResolutionError::StubTypeMissingSymbolTableEntry
      }
    }
  }
}

impl From<types::DirectRecursionCheckError> for TypeStripError {
  fn from(error: types::DirectRecursionCheckError) -> Self {
    match error {
      types::DirectRecursionCheckError::SymbolTableMissingEntry => {
        TypeStripError::SymbolTableMissingEntry
      }
    }
  }
}

#[derive(Debug)]
pub(crate) enum TypeResolutionByIdError {
  MissingEntryForTypeId,
  TypeResolutionError(TypeResolutionError),
}

pub(crate) fn push_to_universe_stack(
  mut universe_stack: UniverseStack,
  new_universe_id: symbol_table::UniverseId,
) -> Result<UniverseStack, &'static str> {
  if universe_stack.contains(&new_universe_id) {
    return Err("universe stack should not contain the new universe id already");
  }

  universe_stack.push(new_universe_id);

  Ok(universe_stack)
}

pub(crate) struct ResolutionHelper<'a> {
  pub base: BaseResolutionHelper<'a>,
  pub type_env: &'a symbol_table::TypeEnvironment,
}

impl<'a> ResolutionHelper<'a> {
  pub fn new(
    universes: &'a instantiation::TypeSchemes,
    symbol_table: &'a symbol_table::SymbolTable,
    type_env: &'a symbol_table::TypeEnvironment,
  ) -> Self {
    let base = BaseResolutionHelper::new(universes, symbol_table);

    Self { base, type_env }
  }

  pub(crate) fn resolve_by_id(
    &'a self,
    type_id: &symbol_table::TypeId,
    universe_stack: UniverseStack,
  ) -> Result<std::borrow::Cow<'a, types::Type>, TypeResolutionByIdError> {
    let ty = self
      .type_env
      .get(type_id)
      .ok_or(TypeResolutionByIdError::MissingEntryForTypeId)?;

    self
      .base
      .resolve(ty, universe_stack)
      .map_err(|type_resolution_error| {
        TypeResolutionByIdError::TypeResolutionError(type_resolution_error)
      })
  }
}

pub(crate) struct BaseResolutionHelper<'a> {
  universes: &'a instantiation::TypeSchemes,
  symbol_table: &'a symbol_table::SymbolTable,
}

impl<'a> BaseResolutionHelper<'a> {
  pub fn new(
    universes: &'a instantiation::TypeSchemes,
    symbol_table: &'a symbol_table::SymbolTable,
  ) -> Self {
    Self {
      universes,
      symbol_table,
    }
  }

  pub(crate) fn get_universes(&self) -> &instantiation::TypeSchemes {
    self.universes
  }

  fn get_in_universe_stack(
    &self,
    key: &symbol_table::SubstitutionId,
    universe_stack: &UniverseStack,
  ) -> Result<Option<&types::Type>, &'static str> {
    // REVIEW: More research and review is needed to ensure that this approach works for all cases. Perhaps implement more tests, but also need to intuitively understand how this works 100%.
    // FIXME: Regarding the case where an infinite loop would occur, what happens then if the search is still done in reverse, but finds no entry at the beginning (goes until the end), then it would reach an infinite loop causing stack-overflow? If that's the case, we'd need an assertion and a minor tweak to prevent such a case. Of course, we'd only use an assertion if such case is considered a logic bug.
    // NOTE: The iteration order here is crucial; by iterating in reverse,
    // closer universes are prioritized. This is important because in the case
    // that an artifact uses a polymorphic item multiple times, at the same time,
    // (ie. `Id<Id<T>>`), the innermost artifact should be granted priority,
    // otherwise, it may lead to a situation where the first artifact keeps
    // on getting selected as the resolution universe, and may lead to an
    // infinite loop (which may cause a stack overflow).
    for universe_id in universe_stack.iter().rev() {
      let universe = self
        .universes
        .get(universe_id)
        .ok_or("universe id missing from type schemes")?;

      if let Some(value) = universe.get(key) {
        return Ok(Some(value));
      }
    }

    Ok(None)
  }

  /// Recursively instantiate stub type if applicable, then substitute it with
  /// a monomorphic type if it is a generic/polymorphic type.
  ///
  /// This will return a concrete type, with a concrete immediate subtree.
  pub(crate) fn resolve<'b>(
    &'b self,
    ty: &'b types::Type,
    universe_stack: UniverseStack,
  ) -> Result<std::borrow::Cow<'b, types::Type>, TypeResolutionError> {
    // Nothing to do if the type is already fully concrete.
    if ty.is_immediate_subtree_concrete() {
      return Ok(std::borrow::Cow::Borrowed(ty));
    }

    let resolution = match ty {
      types::Type::Stub(stub_type) => self.resolve_stub_type(stub_type, universe_stack)?,
      types::Type::Generic(generic_type) => {
        self.resolve_generic(&generic_type.substitution_id, universe_stack)?
      }
      // The type is not a stub, generic (at least at this layer), or a fully concrete type.
      // In other words, the type contains a nested stub, or generic at some level on its
      // subtree.
      _ => self.resolve_within_subtree(ty, universe_stack)?,
    };

    assert!(
      resolution.is_immediate_subtree_concrete(),
      "resolved type should be concrete"
    );

    Ok(resolution)
  }

  fn resolve_within_subtree<'b>(
    &self,
    ty: &types::Type,
    universe_stack: UniverseStack,
  ) -> Result<std::borrow::Cow<'b, types::Type>, TypeResolutionError> {
    Ok(std::borrow::Cow::Owned(match ty {
      types::Type::Pointer(pointee) => types::Type::Pointer(Box::new(
        self.resolve(pointee, universe_stack)?.into_owned(),
      )),
      types::Type::Reference(pointee) => types::Type::Reference(Box::new(
        self.resolve(pointee, universe_stack)?.into_owned(),
      )),
      types::Type::Tuple(tuple) => types::Type::Tuple(types::TupleType(
        tuple
          .0
          .iter()
          // FIXME: Properly handle result.
          // OPTIMIZE: Avoid cloning.
          .map(|ty| self.resolve(ty, universe_stack.clone()))
          .collect::<Result<Vec<_>, _>>()?
          .into_iter()
          .map(|cow| cow.into_owned())
          .collect(),
      )),
      types::Type::Object(object_type) => {
        let fields = object_type.fields.iter().try_fold(
          std::collections::BTreeMap::new(),
          |mut accumulator, field| -> Result<_, TypeResolutionError> {
            accumulator.insert(
              field.0.to_owned(),
              // OPTIMIZE: Avoid cloning.
              self.resolve(field.1, universe_stack.clone())?.into_owned(),
            );

            Ok(accumulator)
          },
        )?;

        types::Type::Object(types::ObjectType {
          fields,
          kind: object_type.kind,
        })
      }
      types::Type::Signature(signature) => {
        let return_type = self
          .resolve(&signature.return_type, universe_stack.clone())?
          .into_owned();

        let parameter_types = signature
          .parameter_types
          .iter()
          // OPTIMIZE: Avoid cloning.
          .map(|param_type| self.resolve(param_type, universe_stack.clone()))
          .collect::<Result<Vec<_>, _>>()?
          .into_iter()
          .map(|cow| cow.into_owned())
          .collect();

        types::Type::Signature(types::SignatureType {
          arity_mode: signature.arity_mode,
          parameter_types,
          return_type: Box::new(return_type),
        })
      }
      _ => unreachable!(
        "type should have been a type constructor by this point, with a nested generic or stub type"
      ),
    }))
  }

  pub(crate) fn resolve_generic<'b>(
    &'b self,
    substitution_id: &'b symbol_table::SubstitutionId,
    universe_stack: UniverseStack,
  ) -> Result<std::borrow::Cow<'b, types::Type>, TypeResolutionError> {
    // Generics cannot be resolved without any context artifact id and
    // thus no corresponding substitution environment.
    if universe_stack.is_empty() {
      return Err(TypeResolutionError::EmptyUniverseStackWhenResolvingGeneric);
    }

    // There should be at least one universe in the universe stack,
    // otherwise it would not be possible to resolve the generic.
    if self.universes.is_empty() {
      return Err(TypeResolutionError::NoUniversesWhenResolvingGeneric);
    }

    // BUG: When generics are nested, say inside pointer types, the inner generic will be called without any context artifact id, because it's not part of a polymorphic function. Instead, what should happen is that this function be called with the stub's artifact id, similar to as it is done on the case above.

    // NOTE: The substituted type might be a type stub, or a generic, which
    // is acceptable. The only job of this logic is to substitute one layer
    // of the type.
    let substitution = match self.get_in_universe_stack(substitution_id, &universe_stack) {
      Ok(Some(substitution)) => substitution,
      _ => return Err(TypeResolutionError::CouldNotFindSubstitutionInAnyUniverseInUniverseStack),
    };

    // TODO: Perform `!occurs_in` check to prevent stack overflow / infinite loop with the substitution and the original type. Use the original type and the substitution as the operands for the `occurs_in` assertion. Since this function is `Result`, return `Err` instead of having an assertion (is it acceptable for a type to resolve to itself?).
    // FIXME: This won't work for this case; recursion cannot be simply detected via stub type stripping, as instantiation is involved at each recursive step! Instead, some sort of state must be passed around resolution steps to detect recursion.
    if substitution.contains_directly_recursive_types(&self.symbol_table)? {
      // TODO: Handle possible case of recursive types.
      todo!();
    }

    let resolution = self.resolve(&substitution, universe_stack)?;

    assert!(
      resolution.is_immediate_subtree_concrete(),
      "resolution of generic type should be concrete"
    );

    Ok(resolution)
  }

  pub(crate) fn resolve_stub_type<'b>(
    &'b self,
    stub_type: &'b types::StubType,
    universe_stack: UniverseStack,
  ) -> Result<std::borrow::Cow<'b, types::Type>, TypeResolutionError> {
    // If the stub type is not polymorphic, simply strip all of its
    // monomorphic layers, and resolve the target type.
    if stub_type.generic_hints.is_empty() {
      assert!(
        !self.universes.contains_key(&stub_type.universe_id),
        "non-artifact stub types should have no entry on the universes"
      );

      let stripped_target = stub_type
        // OPTIMIZE: Avoid cloning.
        .clone()
        .strip_all_monomorphic_stub_layers(self.symbol_table)
        .or(Err(TypeResolutionError::StubTypeMissingSymbolTableEntry))?;

      dbg!(stripped_target.clone());

      let resolved_target = self.resolve(&stripped_target, universe_stack)?;

      // OPTIMIZE: Avoid cloning; currently only cloning to satisfy borrow checker.
      return Ok(std::borrow::Cow::Owned(resolved_target.into_owned()));
    }

    let target_registry_item = self
      .symbol_table
      .follow_link(&stub_type.path.link_id)
      .ok_or(TypeResolutionError::StubTypeMissingSymbolTableEntry)?;

    let associated_universe = self
      .universes
      .get(&stub_type.universe_id)
      .expect("a universe is not registered for the polymorphic stub type artifact");

    assert!(
      !associated_universe.is_empty(),
      "universe associated with polymorphic stub type should not be empty, as it would imply that at least a substitution is missing",
    );

    // By this point, it is known that the stub type is polymorphic, and
    // thus that it has generic hints. Thus, it is appropriate to add its
    // universe id to the universe stack.
    let next_universe_stack =
      push_to_universe_stack(universe_stack, stub_type.universe_id.clone()).unwrap();

    assert!(
      !next_universe_stack.is_empty(),
      "next universe stack should contain at least one universe id when resolving a polymorphic stub type"
    );

    let resolution = match target_registry_item {
      // In the case of the stub type pointing to a generic type, recursively
      // call this function, since another case handles generic types.
      symbol_table::RegistryItem::GenericType(generic_type) => {
        // BUG: When the pointer type is lowered, it calls for its pointee type to be lowered, which is then resolved (without context artifact id). At this point, a stub to a generic is being resolved. In that case/branch of the if statement of this function, the current artifact id (None) is passed in to resolve the generic. Since it is a generic, it requires a context artifact id, and thus fails. That generic should be resolved with the stub's artifact id.
        self.resolve_generic(&generic_type.substitution_id, next_universe_stack)?
      }
      // The body itself might be a nested generic or stub type, and thus
      // it needs to be recursively resolved.
      symbol_table::RegistryItem::TypeDef(type_def) => {
        // NOTE: The generic parameters of the type def. are not relevant here,
        // regardless of the fact that the type def. is known to be polymorphic
        // by this point. This is because neither unification of parameters vs.
        // hints, or substitution of parameters for hints is needed here. Instead,
        // that process has already taken place during unification, and the result
        // is a universe, which is then used during this resolution step.

        assert!(
          stub_type.generic_hints.len() == type_def.generics.parameters.len(),
          "generic hints and parameters should have the same length as a sanity check"
        );

        self.resolve(&type_def.body, next_universe_stack)?
      }
      // TODO: Handle unions.
      symbol_table::RegistryItem::Union(..) => todo!("unions are not yet handled"),
      _ => unreachable!("stub type should only point to a generic, type def., or union target"),
    };

    // REVIEW: How can this assertion ever be satisfied, if at any resolution step, this and other resolution functions are always used? In other words, at what point does substitution occur? Once found out, make sure to add a comment explaining it.
    assert!(
      resolution.is_immediate_subtree_concrete(),
      "resolved type should be concrete"
    );

    Ok(resolution)
  }
}
