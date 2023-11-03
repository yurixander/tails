//! The instantiation step encompasses the specialization and validation of polymorphic
//! constructs and types, since they are essentially ignored during regular unification.
//! The reason that they are ignored is because by that phase, generic types have no
//! corresponding instantiations.
//!
//! This phase determines instantiations of polymorphic constructs such as polymorphic
//! functions, and creates a generic substitution environment per instance invoker (ie.
//! a call site to a polymorphic function) that can be retrieved by subsequent phases.

use crate::{
  assert_extract, diagnostic, inference, instantiation, resolution, substitution, symbol_table,
  types,
};

pub struct TypeUnificationContext<'a> {
  pub(crate) symbol_table: &'a symbol_table::SymbolTable,
  /// Substitution map for type variables and generics.
  substitutions: symbol_table::SubstitutionEnv,
  object_substitutions: symbol_table::SubstitutionEnv,
  resolution_helper: resolution::BaseResolutionHelper<'a>,
}

impl<'a> TypeUnificationContext<'a> {
  pub fn new(
    symbol_table: &'a symbol_table::SymbolTable,
    type_var_substitutions: symbol_table::SubstitutionEnv,
    universes: &'a instantiation::TypeSchemes,
  ) -> Self {
    Self {
      symbol_table,
      substitutions: type_var_substitutions,
      object_substitutions: symbol_table::SubstitutionEnv::new(),
      resolution_helper: resolution::BaseResolutionHelper::new(universes, symbol_table),
    }
  }

  /// Attempt to substitute an object type with its corresponding substitution
  /// if any is registered. This is used for when processing object types during
  /// unification, since the unification algorithm requires that the types being
  /// unified are their most recent versions. Without substitution, the wrong object
  /// type would be unified instead.
  pub(crate) fn substitute_object(&self, object: &'a types::ObjectType) -> &types::ObjectType {
    let substitution_id = match object.kind {
      types::ObjectKind::Open(substitution_id) => substitution_id,
      types::ObjectKind::Closed => return object,
    };

    let substitution = match self.substitutions.get(&substitution_id) {
      Some(substitution) => substitution,
      None => return object,
    };

    // SAFETY: Will there ever be a case where substitution will need to be applied more than a single level of depth? If not, remove the recursive call.
    // Object types' substitutions can only be other object types.
    self.substitute_object(assert_extract!(substitution, types::Type::Object))
  }

  /// Recursively check if a type variable occurs within a type's substitution
  /// path.
  ///
  /// For this to be `true`, the type in question must be a type variable.
  /// Any other type will yield `false`. In other words, stub and generic types
  /// will not be resolved.
  ///
  /// This is used to avoid constructing infinite types.
  pub(crate) fn occurs_in(
    &self,
    subject_id: &symbol_table::SubstitutionId,
    ty: &types::Type,
  ) -> Result<bool, &'static str> {
    const NO_TYPE_VAR_SUBSTITUTION: &str =
      "substitution environment does not contain a substitution for type variable";

    match ty {
      types::Type::Variable(types::TypeVariable {
        substitution_id, ..
      }) if !self
        .substitutions
        .get(substitution_id)
        .ok_or(NO_TYPE_VAR_SUBSTITUTION)?
        .is_same_type_variable_as(substitution_id) =>
      {
        let substitution = self
          .substitutions
          .get(substitution_id)
          .ok_or(NO_TYPE_VAR_SUBSTITUTION)?;

        self.occurs_in(subject_id, &substitution)
      }
      types::Type::Variable(types::TypeVariable {
        substitution_id, ..
      }) => Ok(substitution_id == subject_id),
      // If the type is not a type variable, simply check that the subject
      // type does not occur within any of its inner types.
      _ => ty.get_inner_types().try_fold(false, |found, inner_type| {
        if found {
          Ok(true)
        } else {
          self.occurs_in(subject_id, inner_type)
        }
      }),
    }
  }

  /// Solves constraints by invoking and performing the unification
  /// algorithm for all registered constraints. Returns a type map
  /// with the solutions.
  ///
  /// This occurs after all the constraints have been added,
  /// and is the last step for Hindley-Milner type inference.
  ///
  /// This will also update the type cache with the resulting types
  /// by performing substitution.
  ///
  /// The given type environment is considered 'partial' because it consists
  /// of type variables, concrete types, and generics. The job of this function
  /// is to solve all type variables, and some generics. Note that not all generics
  /// will be resolved, and that is expected because polymorphic functions must remain
  /// with their generic types.
  pub(crate) fn solve_constraints(
    &mut self,
    partial_type_env: &symbol_table::TypeEnvironment,
    constraints: &inference::ConstraintSet,
  ) -> diagnostic::Maybe<symbol_table::TypeEnvironment> {
    // SAFETY: What if we have conflicting constraints? Say, we have different calls with different types to the same function? Or if the parameters are constrained to be something, yet the arguments are constrained to be different?
    let constraints = constraints
      .iter()
      // OPTIMIZE: Avoid cloning.
      .cloned()
      .filter(|constraint| matches!(constraint.1, inference::Constraint::Equality(..)))
      .collect::<Vec<_>>();

    let mut diagnostics_helper = diagnostic::DiagnosticsHelper::default();

    // Solve all equality constraints.
    for (universe_stack, constraint) in constraints.clone() {
      assert!(
        universe_stack.len() <= self.resolution_helper.get_universes().len(),
        "there should not be more universes in the universe stack than there are in the type schemes, otherwise it would mean that the type schemes are not exhaustive, and that a universe is missing (more artifacts than universes?)"
      );

      diagnostics_helper.extend(self.dispatch_constraint(&universe_stack, constraint))?;
    }

    let mut solutions = symbol_table::TypeEnvironment::new();

    let substitution_helper = substitution::UnificationSubstitutionHelper {
      symbol_table: self.symbol_table,
      substitution_env: &self.substitutions,
    };

    // FIXME: Need to handle the case in which a non-monomorphic type stub targets a polymorphic type def (generic hint count mismatch). Or it might be already handled; but need to verify this for stubs! That may be handled here or elsewhere (consideration needed). It would not be an assertion; it is a possible input of the user, and thus should be handled via diagnostics.

    // Substitute all types in the substitution map, and store the results on the
    // solutions map to be returned. In the case that any solving fails, issue a
    // corresponding diagnostic.
    for (id, ty) in partial_type_env {
      let substitution = match substitution_helper.substitute(ty) {
        Ok(substitution) => substitution,
        // REVISE: Don't just return this error; add it to the diagnostics helper, and return the diagnostics helper.
        Err(substitution::SubstitutionError::TypeStripError(types::TypeStripError::RecursionDetected)) => return Err(vec![diagnostic::Diagnostic::RecursiveType(ty.to_owned())]),
        // This would constitute a logic bug in where the name resolution pass
        // did not properly fill in all entries.
        Err(substitution::SubstitutionError::TypeStripError(types::TypeStripError::SymbolTableMissingEntry)) | Err(substitution::SubstitutionError::DirectRecursionCheckError(types::DirectRecursionCheckError::SymbolTableMissingEntry)) => unreachable!("name resolution should have previously registered all links and nodes in the symbol table")
      };

      // REVISE: Perform stub type stripping on each unification call step instead of everywhere else. This way, there shouldn't need to be a need to strip stub types on subsequent phases after unification has occurred (including here).
      let stripped_substitution = substitution
        .try_strip_all_monomorphic_stub_layers(self.symbol_table)
        // FIXME: Properly handle result.
        .unwrap();

      // There should not be any type variables left after substitution,
      // otherwise it would mean that not all constraints could be fully
      // solved. It possibly means that the user may need to provide more
      // type hints. For example, the usage of the `null` value without
      // any constraints would result in an unsolved type variable for that
      // `null` value's type.
      for inner_type in stripped_substitution
        .get_immediate_subtree_iter()
        // Include the substituted type as well, to ensure that it isn't
        // a type variable itself.
        .chain(std::iter::once(&stripped_substitution))
      {
        if let types::Type::Variable(type_variable) = inner_type {
          diagnostics_helper.add_one(diagnostic::Diagnostic::UnsolvedTypeVariable(
            type_variable.substitution_id,
            type_variable.debug_name.to_string(),
          ));
        }
      }

      // SAFETY: Check that there aren't any type variables on the INDIRECT subtree left?

      solutions.insert(*id, stripped_substitution);
    }

    diagnostics_helper.try_return_value(solutions)
  }

  fn unify_tuple_element_of(
    &mut self,
    tuple_type: &types::Type,
    element_type: &types::Type,
    index: u32,
  ) -> diagnostic::Maybe {
    // TODO: Implement. Might need to occur after equality constraints, so that it doesn't have to deal with type variables, generics, and stub types?
    todo!();
  }

  fn dispatch_constraint(
    &mut self,
    universe_stack: &resolution::UniverseStack,
    constraint: inference::Constraint,
  ) -> diagnostic::Maybe {
    match &constraint {
      // Equality between two types.
      inference::Constraint::Equality(type_a, type_b) => self.unify(type_a, type_b, universe_stack),
      inference::Constraint::TupleElementOf {
        tuple_type,
        element_type,
        index,
      } => self.unify_tuple_element_of(tuple_type, element_type, *index),
    }
  }
}

impl TypeUnificationContext<'_> {
  /// Unifies two types for equality. Solves the constraints by performing a
  /// unification algorithm. The types are compared for equivalence, similar
  /// to a system of equations, type variables are *substituted* to aid the
  /// unification process.
  ///
  /// This serves as one step of the type inference substitution algorithm.
  /// This also checks for compatibility among types. If they are compatible,
  /// they will be inserted into the substitution map, which serves as the
  /// container for the *solutions*.
  ///
  /// If at any point the processes fails to determine the equivalence of
  /// two type variables, a diagnostic is issued and appended to the
  /// diagnostics list.
  ///
  /// If one or both given types are *partial* (ie. stub types), they will be
  /// fully resolved into concrete types before continuing to unify.
  pub(crate) fn unify(
    &mut self,
    type_a: &types::Type,
    type_b: &types::Type,
    universe_stack: &resolution::UniverseStack,
  ) -> diagnostic::Maybe {
    // CONSIDER: Since various types have substitution ids, consider creating a `find_substitution_id` for types and resolving it automatically here on top, then removing the resolution logic from the match cases (this simplifies and standardizes the substitution procedure). Then, on the actual match cases, if they're reached it means that substitution couldn't be performed, thus we just have that logic for when they couldn't be substituted there (if any). This will also make it much easier to implement new types that may require substitution. The logic for when the substitution is itself will also need to added, to avoid infinite loops. The same abstraction can be used for the occurs check.

    // TODO: Add an example of a case to demonstrate why this is the case (order matters for match cases), and explain clearly in which path what should occur and why.
    // NOTE: The order of match cases is important and can affect the unification
    // algorithm.
    match (type_a, type_b) {
      // NOTE: Type variables should ALWAYS be unified before the
      // other cases, otherwise it may miss opportunities to update
      // substitutions for type variables.
      (types::Type::Variable(type_variable), other)
      | (other, types::Type::Variable(type_variable)) => {
        self.unify_type_variable(type_variable, other, universe_stack)
      }
      (types::Type::Opaque, types::Type::Opaque) => Ok(()),
      (types::Type::Unit, types::Type::Unit) => Ok(()),
      (types::Type::Stub(stub), other) | (other, types::Type::Stub(stub)) => {
        self.unify_stub(stub, other, universe_stack)
      }
      (types::Type::Generic(generic), other) | (other, types::Type::Generic(generic)) => {
        self.unify_generic(generic, other, universe_stack)
      }
      (types::Type::Tuple(tuple_a), types::Type::Tuple(tuple_b)) => {
        self.unify_tuples(tuple_a, tuple_b, universe_stack)
      }
      (types::Type::Pointer(pointee_a), types::Type::Pointer(pointee_b)) => {
        self.unify(pointee_a.as_ref(), pointee_b.as_ref(), &universe_stack)
      }
      (types::Type::Signature(signature_a), types::Type::Signature(signature_b)) => {
        self.unify_signatures(signature_a, signature_b, universe_stack)
      }
      (types::Type::Reference(pointee_a), types::Type::Reference(pointee_b)) => {
        self.unify(&pointee_a, &pointee_b, &universe_stack)
      }
      // Opaque pointers are incompatible with typed pointers. They must be casted
      // before any pointer operation is performed on them.
      (types::Type::Opaque, types::Type::Pointer(_))
      | (types::Type::Pointer(_), types::Type::Opaque) => {
        Err(vec![diagnostic::Diagnostic::OpaquePointerMustBeCasted])
      }
      (types::Type::Object(object_a), types::Type::Object(object_b)) => {
        self.unify_objects(object_a, object_b, universe_stack)
      }
      (types::Type::Union(union_a), types::Type::Union(union_b)) => {
        if union_a.registry_id != union_b.registry_id {
          Err(vec![diagnostic::Diagnostic::UnionTypesDiffer])
        } else {
          // TODO: For now, we might not need to unify variants. However, when we do add generics we might have to.

          Ok(())
        }
      }
      (types::Type::Primitive(primitive_a), types::Type::Primitive(primitive_b)) => {
        if primitive_a != primitive_b {
          Err(vec![diagnostic::Diagnostic::TypeMismatch(
            type_a.to_owned(),
            type_b.to_owned(),
          )])
        } else {
          Ok(())
        }
      }
      _ => Err(vec![diagnostic::Diagnostic::TypeMismatch(
        type_a.to_owned(),
        type_b.to_owned(),
      )]),
    }
  }

  pub(crate) fn unify_objects(
    &mut self,
    raw_object_a: &types::ObjectType,
    raw_object_b: &types::ObjectType,
    universe_stack: &resolution::UniverseStack,
  ) -> diagnostic::Maybe {
    // REVISE: Abstracting these repetitive substitution logic of multiple types to just finding the substitution id and substituting then re-trying unification if a substitution does exist and it isn't the same as the original type.
    // OPTIMIZE: Avoid cloning.
    let object_a = self.substitute_object(raw_object_a).to_owned();
    let object_b = self.substitute_object(raw_object_b).to_owned();

    let intersection = object_a
      .fields
      .iter()
      .flat_map(|field_a| {
        object_b
          .fields
          .get(field_a.0)
          .map(|type_b| (field_a.1, type_b))
      })
      .collect::<Vec<_>>();

    let mut diagnostics_helper = diagnostic::DiagnosticsHelper::default();

    // Regardless of the kind of objects, their intersecting fields
    // should always match and thus be unified.
    for (field_a, field_b) in &intersection {
      diagnostics_helper.extend(self.unify(field_a, field_b, &universe_stack))?;
    }

    // TODO: Add passing tests representing each and every single case and edge case outlined here.
    let result = match (object_a.kind, object_b.kind) {
      // If they're both open object types, replace their types in the environment
      // to be the a new open object type, representing the union of both.
      (types::ObjectKind::Open(substitution_id_a), types::ObjectKind::Open(substitution_id_b)) => {
        let union: types::ObjectFieldMap = object_a
          .fields
          .iter()
          .chain(object_b.fields.iter())
          .map(|field| (field.0.to_owned(), field.1.to_owned()))
          .collect();

        self.object_substitutions.insert(
          substitution_id_a,
          types::Type::Object(types::ObjectType {
            fields: union.clone(),
            kind: types::ObjectKind::Open(substitution_id_a),
          }),
        );

        self.object_substitutions.insert(
          substitution_id_b,
          types::Type::Object(types::ObjectType {
            fields: union,
            kind: types::ObjectKind::Open(substitution_id_b),
          }),
        );

        Ok(())
      }
      // If one is open and the other is closed, ensure that all open fields are
      // also present in the closed object's fields. This is because closed object
      // types must contain all fields of the open object.
      (types::ObjectKind::Open(substitution_id), types::ObjectKind::Closed) => {
        self.check_open_closed_objects(&object_a, &object_b, substitution_id)
      }
      (types::ObjectKind::Closed, types::ObjectKind::Open(substitution_id)) => {
        self.check_open_closed_objects(&object_b, &object_a, substitution_id)
      }
      // Otherwise, if they're both closed, simply ensure that the field count matches.
      // Also, the intersection must be the same length as any of the field's lengths
      // (since they would be checked to ensure they are the same). In other words,
      // `len(intersection) == len(a) == len(b)` must hold true.
      (types::ObjectKind::Closed, types::ObjectKind::Closed) => {
        if intersection.len() != object_a.fields.len() {
          Err(vec![
            diagnostic::Diagnostic::IntersectionOfClosedObjectsIsIncomplete(
              intersection.len(),
              object_a.fields.len(),
            ),
          ])
        } else if object_a.fields.len() != object_b.fields.len() {
          Err(vec![diagnostic::Diagnostic::ObjectFieldCountMismatch(
            object_a.fields.len(),
            object_b.fields.len(),
          )])
        } else {
          Ok(())
        }
      }
    };

    diagnostics_helper.extend(result)
  }

  fn unify_stub(
    &mut self,
    stub_type: &types::StubType,
    other: &types::Type,
    universe_stack: &resolution::UniverseStack,
  ) -> diagnostic::Maybe {
    let strip_result = stub_type
      // OPTIMIZE: Avoid cloning.
      .to_owned()
      .strip_all_monomorphic_stub_layers(self.symbol_table);

    let stripped_target = match strip_result {
      Ok(stripped_target) => stripped_target,
      Err(types::TypeStripError::RecursionDetected) => {
        return Err(vec![diagnostic::Diagnostic::RecursiveType(
          types::Type::Stub(stub_type.to_owned()),
        )]);
      }
      Err(types::TypeStripError::SymbolTableMissingEntry) => {
        // REVISE: Find a way to use `auxiliary::BUG_RESOLUTION` instead.
        unreachable!("name resolution should have previously registered all links and nodes in the symbol table")
      }
    };

    if let types::Type::Stub(polymorphic_stub_type) = &stripped_target {
      // NOTE: No need to include the stub type's universe id as part of the
      // initial universe stack, as the resolution function already inserts it.
      let resolution = self
        .resolution_helper
        .resolve_stub_type(polymorphic_stub_type, universe_stack.clone())
        .unwrap()
        // OPTIMIZE: Any way to avoid cloning? Possibly accept `std::borrow::Cow` on the `unify` function, or would that be too much?
        .into_owned();

      // Continue unification, but against the stub type's resolution.
      return self.unify(&resolution, other, universe_stack);
    }

    // REVIEW: What if the target is an artifact that accepts generics, but none were provided? Should that be reported here?
    self.unify(&stripped_target, other, universe_stack)
  }

  fn check_arity_mode_requirements(
    signature_a: &types::SignatureType,
    signature_b: &types::SignatureType,
  ) -> bool {
    // Neither signature is variadic, so there is nothing to check.
    // All requirements are met.
    if !signature_a.arity_mode.is_variadic() && !signature_b.arity_mode.is_variadic() {
      return true;
    }
    // In case where they are both variadic, their minimum parameter requirement
    // must match.
    else if signature_a.arity_mode.is_variadic() && signature_b.arity_mode.is_variadic() {
      // REVIEW: Is this a bug or an expected, valid input? If it's not a bug, it should be using `Result` instead.
      assert!(signature_a
        .arity_mode
        .get_minimum_required_parameters()
        .is_some());

      // REVIEW: Is this a bug or an expected, valid input? If it's not a bug, it should be using `Result` instead.
      assert!(signature_b
        .arity_mode
        .get_minimum_required_parameters()
        .is_some());

      return signature_a
        .arity_mode
        .get_minimum_required_parameters()
        .eq(&signature_b.arity_mode.get_minimum_required_parameters());
    }

    // By this point, only one of the two signatures is variadic, while the other
    // one is guaranteed to be fixed.

    let variadic_signature = if signature_a.arity_mode.is_variadic() {
      signature_a
    } else {
      signature_b
    };

    let non_variadic_signature = if !signature_a.arity_mode.is_variadic() {
      signature_a
    } else {
      signature_b
    };

    if let types::ArityMode::Variadic {
      minimum_required_parameters,
    } = &variadic_signature.arity_mode
    {
      // The minimum parameter count requirement is not satisfied.
      if non_variadic_signature.parameter_types.len() < *minimum_required_parameters {
        return false;
      }
    }

    true
  }

  pub(crate) fn unify_signatures(
    &mut self,
    signature_a: &types::SignatureType,
    signature_b: &types::SignatureType,
    universe_stack: &resolution::UniverseStack,
  ) -> diagnostic::Maybe {
    let is_any_variadic =
      signature_a.arity_mode.is_variadic() || signature_b.arity_mode.is_variadic();

    let signature_a_len = signature_a.parameter_types.len();
    let signature_b_len = signature_b.parameter_types.len();

    let parameter_count_mismatch_error = Err(vec![
      diagnostic::Diagnostic::SignaturesDifferInParameterCount(signature_a_len, signature_b_len),
    ]);

    // If neither function is variadic, their parameter count must match exactly.
    if !is_any_variadic && signature_a_len != signature_b_len {
      return parameter_count_mismatch_error;
    }
    // If one signature is variadic, and the other isn't, then
    // the fixed signature must meet the minimum amount of required
    // parameters of the variadic signature (if that minimum amount
    // is present). Or if they're both variadic, their minimum parameter
    // requirement must match. This branch handles both cases.
    else if !Self::check_arity_mode_requirements(signature_a, signature_b) {
      return parameter_count_mismatch_error;
    }

    let mut diagnostics_helper = diagnostic::DiagnosticsHelper::default();

    // NOTE: The zip will ignore variadic parameters without pairs.
    for (parameter_a, parameter_b) in signature_a
      .parameter_types
      .iter()
      .zip(signature_b.parameter_types.iter())
    {
      diagnostics_helper.extend(self.unify(parameter_a, parameter_b, universe_stack))?;
    }

    diagnostics_helper.extend(self.unify(
      signature_a.return_type.as_ref(),
      signature_b.return_type.as_ref(),
      universe_stack,
    ))
  }

  pub(crate) fn unify_tuples(
    &mut self,
    tuple_a: &types::TupleType,
    tuple_b: &types::TupleType,
    universe_stack: &resolution::UniverseStack,
  ) -> diagnostic::Maybe {
    let mut diagnostics_helper = diagnostic::DiagnosticsHelper::default();
    let types_a = &tuple_a.0;
    let types_b = &tuple_b.0;

    if types_a.len() != types_b.len() {
      diagnostics_helper.try_add_one(diagnostic::Diagnostic::TuplesDifferInLength)?;
    }

    for (type_a, type_b) in types_a.iter().zip(types_b.iter()) {
      diagnostics_helper.extend(self.unify(type_a, type_b, universe_stack))?;
    }

    diagnostics_helper.check()
  }

  pub(crate) fn unify_type_variable(
    &mut self,
    type_variable: &types::TypeVariable,
    other_type: &types::Type,
    universe_stack: &resolution::UniverseStack,
  ) -> diagnostic::Maybe {
    // If both types are the same type variable do nothing as
    // they are equivalent; there is no need to verify anything further.
    if other_type.is_same_type_variable_as(&type_variable.substitution_id) {
      return Ok(());
    }
    // If there is an existing substitution for the type
    // variable, unify with that instead. This way we get
    // rid of type variables before actual unification.
    else if let Some(existing_substitution) =
      type_variable.try_substitute_self(&self.substitutions)
    {
      // OPTIMIZE: Avoid cloning.
      return self.unify(
        &existing_substitution.to_owned(),
        other_type,
        universe_stack,
      );
    }
    // If it is a type variable which is bound in the substitution
    // environment, unify with the binding instead. This way we get
    // rid of type variables before actual unification takes place,
    // and work with concrete types instead.
    else if let types::Type::Variable(other_type_var) = other_type {
      if let Some(other_substitution) = other_type_var.try_substitute_self(&self.substitutions) {
        return self.unify_type_variable(
          type_variable,
          // OPTIMIZE: Avoid redundant cloning.
          &other_substitution.to_owned(),
          universe_stack,
        );
      }
    }

    // Otherwise, the other type is an unbound type variable; update the
    // substitution of the unbound type variable.

    // Remember to do an occurs check to avoid constructing infinite types.
    if self
      .occurs_in(&type_variable.substitution_id, &other_type)
      .unwrap()
    {
      return Err(vec![diagnostic::Diagnostic::ConstructionOfInfiniteType]);
    }

    self
      .substitutions
      .insert(type_variable.substitution_id, other_type.to_owned());

    Ok(())
  }

  pub(crate) fn unify_generic(
    &mut self,
    generic_type: &types::GenericType,
    other_type: &types::Type,
    universe_stack: &resolution::UniverseStack,
  ) -> diagnostic::Maybe {
    // NOTE: It makes no difference if both types are generic types,
    // even if they are from different groups/contexts. There are valid
    // cases where generic types from different contexts are unified against
    // each other, such as when a generic type is passed as a generic hint to
    // polymorphic functions.

    assert!(
      !universe_stack.is_empty(),
      "universe stack should not be empty when unifying and resolving a generic type"
    );

    let resolution = self
      .resolution_helper
      .resolve_generic(&generic_type.substitution_id, universe_stack.clone())
      .unwrap()
      .into_owned();

    assert!(
      resolution.is_immediate_subtree_concrete(),
      "resolution of generic type should be concrete"
    );

    self.unify(&resolution, other_type, universe_stack)
  }

  pub(crate) fn check_open_closed_objects(
    &mut self,
    open_object: &types::ObjectType,
    closed_object: &types::ObjectType,
    substitution_id: symbol_table::SubstitutionId,
  ) -> diagnostic::Maybe {
    // Closed object must contain all fields of the open object.
    for open_field in &open_object.fields {
      if !closed_object.fields.contains_key(open_field.0) {
        return Err(vec![diagnostic::Diagnostic::ObjectFieldDoesNotExist(
          open_field.0.to_owned(),
        )]);
      }
    }

    self.substitutions.insert(
      substitution_id,
      types::Type::Object(types::ObjectType {
        fields: closed_object.fields.clone(),
        // When open objects are unified against closed objects, they are
        // no longer open to extensibility.
        kind: types::ObjectKind::Closed,
      }),
    );

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::auxiliary;

  #[test]
  fn occurs_in() {
    let mut id_generator = auxiliary::IdGenerator::default();
    let first_index_id = id_generator.next_substitution_id();
    let second_index_id = id_generator.next_substitution_id();
    let symbol_table = symbol_table::SymbolTable::default();
    let universes = instantiation::TypeSchemes::new();

    let mut type_unification_context = TypeUnificationContext::new(
      &symbol_table,
      symbol_table::SubstitutionEnv::new(),
      &universes,
    );

    type_unification_context.substitutions.insert(
      first_index_id.clone(),
      types::Type::Variable(types::TypeVariable {
        substitution_id: first_index_id.clone(),
        debug_name: "test",
      }),
    );

    type_unification_context
      .substitutions
      .insert(second_index_id.clone(), types::Type::Unit);

    let subject_type_variable = types::Type::Variable(types::TypeVariable {
      substitution_id: first_index_id,
      debug_name: "test",
    });

    assert_eq!(
      type_unification_context.occurs_in(&first_index_id, &subject_type_variable),
      Ok(true)
    );

    assert_eq!(
      type_unification_context.occurs_in(&second_index_id, &subject_type_variable),
      Ok(false)
    );

    assert_eq!(
      type_unification_context.occurs_in(&first_index_id, &types::Type::Unit),
      Ok(false)
    );
  }

  #[test]
  fn solve_constraints() {
    let symbol_table = symbol_table::SymbolTable::default();
    let universes = instantiation::TypeSchemes::new();

    let mut unification_ctx = TypeUnificationContext::new(
      &symbol_table,
      symbol_table::SubstitutionEnv::new(),
      &universes,
    );

    // TODO: Add actual constraints to complete this test.

    assert!(unification_ctx
      .solve_constraints(
        &symbol_table::TypeEnvironment::new(),
        &inference::ConstraintSet::new()
      )
      .is_ok());
  }
}
