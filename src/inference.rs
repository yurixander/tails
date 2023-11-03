use crate::{assert_extract, ast, auxiliary, resolution, symbol_table, types};

pub type ConstraintSet = Vec<(resolution::UniverseStack, Constraint)>;

pub(crate) struct InferenceResult {
  pub constraints: ConstraintSet,
  pub universe_id: Option<symbol_table::UniverseId>,
  pub type_var_substitutions: symbol_table::SubstitutionEnv,
  pub type_env: symbol_table::TypeEnvironment,
  pub ty: types::Type,
  pub id_count: usize,
}

pub(crate) struct InferenceOverallResult {
  pub constraints: ConstraintSet,
  pub type_var_substitutions: symbol_table::SubstitutionEnv,
  pub type_env: symbol_table::TypeEnvironment,
  pub next_id_count: usize,
}

pub(crate) struct InferenceContext<'a> {
  /// Constraints are expectations, or hints, of equality between a pair of types.
  ///
  /// They are first gathered, then the unification algorithm is performed to solve types, at
  /// the last step of type inference.
  constraints: ConstraintSet,
  universe_stack: resolution::UniverseStack,
  own_universe_id: Option<symbol_table::UniverseId>,
  id_generator: auxiliary::IdGenerator,
  /// A type environment, exclusively associating type variables with their
  /// corresponding substitutions.
  ///
  /// Substitutions are only used by the unification algorithm, it does not
  /// represent the types of nodes or expressions. This substitution environment
  /// is subsequently fed into the type unification context, since it registers
  /// all type variables that were created on this type inference context.
  type_var_substitutions: symbol_table::SubstitutionEnv,
  /// A type map that associates nodes with their corresponding monomorphic types.
  ///
  /// Before unification, associated types may be in a *partial* state;
  /// they may have the form of type variables or stubs. After unification,
  /// all associated types are *monomorphic* (concrete).
  ///
  /// Post-unification, all types stored in this environment have been unified, and are
  /// monomorphic. It contains no type variable substitutions or meta types.
  type_env: symbol_table::TypeEnvironment,
  symbol_table: &'a symbol_table::SymbolTable,
}

impl<'a> InferenceContext<'a> {
  pub(crate) fn new(
    symbol_table: &'a symbol_table::SymbolTable,
    universe_id: Option<symbol_table::UniverseId>,
    initial_id_count: usize,
  ) -> Self {
    Self {
      symbol_table,
      own_universe_id: universe_id,
      constraints: ConstraintSet::new(),
      universe_stack: resolution::UniverseStack::new(),
      id_generator: auxiliary::IdGenerator::new(initial_id_count),
      type_var_substitutions: symbol_table::SubstitutionEnv::new(),
      type_env: symbol_table::TypeEnvironment::new(),
    }
  }

  pub(crate) fn inherit(&self, child_universe_id: Option<symbol_table::UniverseId>) -> Self {
    let mut universe_stack = self.universe_stack.clone();

    if let Some(parent_universe_id) = &self.own_universe_id {
      assert!(
        !universe_stack.contains(&parent_universe_id),
        "the same construct should not be inferred twice on the same call stack (infinite loop?)"
      );

      universe_stack.push(parent_universe_id.to_owned());
    }

    Self {
      symbol_table: self.symbol_table,
      // BUG: (test:binding) Because `.inherit` is called as the first thing, say on the call site inference function, the context gains the call site's universe id. Which means that constraints created for that context include the call site's universe id, for example, it's arguments! Its arguments should NOT contain the call site's universe id, only its callee when inferred, and also any 'left over' callee inference result types (which may be managed through 'catch-all' proxy functions here).
      own_universe_id: child_universe_id,
      universe_stack,
      constraints: ConstraintSet::new(),
      id_generator: auxiliary::IdGenerator::new(self.id_generator.get_counter()),
      type_var_substitutions: symbol_table::SubstitutionEnv::new(),
      type_env: symbol_table::TypeEnvironment::new(),
    }
  }

  pub(crate) fn into_overall_result(self) -> InferenceOverallResult {
    InferenceOverallResult {
      constraints: self.constraints,
      type_var_substitutions: self.type_var_substitutions,
      type_env: self.type_env,
      next_id_count: self.id_generator.get_counter(),
    }
  }

  /// Create a signature type from the given signature and return type.
  ///
  /// The return type id is registered in the type cache.
  ///
  /// This is a convenience method for creating a function types, that will take
  /// into account generics and type hints. Logic for instance parameters is not considered.
  pub(crate) fn create_signature_type(
    &mut self,
    signature: &ast::Signature,
  ) -> types::SignatureType {
    let return_type = if let Some(return_type_hint) = &signature.return_type_hint {
      return_type_hint.to_owned()
    } else {
      self.create_type_variable("signature.return_type")
    };

    // SAFETY: Should there be a debugging assertion ensuring that the signature's return type id has no corresponding entry on the type environment? But, if the function is inferred more than once, it would be indeed inserted multiple times. If so, make a note here of that fact.
    self
      .type_env
      .insert(signature.return_type_id, return_type.to_owned());

    let parameter_types = signature
      .parameters
      .iter()
      .map(|parameter| self.visit(parameter.as_ref()))
      .collect::<Vec<_>>();

    types::SignatureType {
      // NOTE: Since this function is used to create signature types for
      // functions and closures only, and they cannot be variadic, the
      // variadic status should remain as non-variadic.
      arity_mode: types::ArityMode::Fixed,
      parameter_types,
      return_type: Box::new(return_type.to_owned()),
    }
  }

  pub(crate) fn visit_target_via_link(
    &mut self,
    link_id: &symbol_table::LinkId,
  ) -> Result<types::Type, &'static str> {
    let target = self
      .symbol_table
      .follow_link(link_id)
      .ok_or(auxiliary::MISSING_SYMBOL_TABLE_ENTRY)?;

    let target_item = target.into_item().ok_or("target is not an item")?;

    // NOTE: The target's type should not be cached since the expected type
    // might be different, regardless of whether multiple references point to
    // the same target node. For example, this is crucial when dealing with
    // polymorphic functions.
    Ok(self.visit(&target_item))
  }

  pub(crate) fn determine_arity_mode_for_callable(
    &self,
    callable: &ast::Callable,
  ) -> types::ArityMode {
    match callable {
      ast::Callable::ForeignFunction(foreign_function) => {
        if foreign_function.signature.is_variadic {
          types::ArityMode::Variadic {
            minimum_required_parameters: foreign_function.signature.parameters.len(),
          }
        } else {
          types::ArityMode::Fixed
        }
      }
      _ => types::ArityMode::Fixed,
    }
  }

  /// Create a fresh type variable and register it on the substitution map.
  ///
  /// Type variables represent unsolved types, and are used in the unification
  /// algorithm to solve constraints.
  pub(crate) fn create_type_variable(&mut self, debug_name: &'static str) -> types::Type {
    let substitution_id = self.id_generator.next_substitution_id();

    let type_variable = types::Type::Variable(types::TypeVariable {
      substitution_id,
      debug_name,
    });

    assert!(
      !self.type_var_substitutions.contains_key(&substitution_id),
      "all newly created type variables should have a unique substitution id (id count not updated?)"
    );

    self
      .type_var_substitutions
      .insert(substitution_id, type_variable.clone());

    type_variable
  }

  pub(crate) fn transient(&self, inferable: &impl Infer<'a>) -> InferenceResult {
    let mut context = self.inherit(None);
    let result = inferable.infer(&context);
    let ty = result.ty.clone();

    context.extend(result);

    context.finalize(ty)
  }

  pub(crate) fn visit(&mut self, inferable: &impl Infer<'a>) -> types::Type {
    let result = inferable.infer(self);
    let ty = result.ty.clone();

    self.extend(result);

    ty
  }

  pub(crate) fn constrain(&mut self, inferable: &impl Infer<'a>, ty: types::Type) -> types::Type {
    let result = inferable.infer(self);
    let mut constraint_universe_stack = self.universe_stack.clone();

    // If the inference result contained a universe id, add it to the
    // universe stack which will be associated with the constraint to be
    // created. Note that such universe id does not affect the state's
    // universe stack, it is only used for the constraint.
    if let Some(universe_id) = &result.universe_id {
      assert!(!constraint_universe_stack.contains(&universe_id));
      constraint_universe_stack.push(universe_id.to_owned());
    }

    // Any constraints created should include the current context's
    // universe id, in case that they are an artifact. For example, for
    // call sites to polymorphic functions, since they create a signature
    // type to constrain against their callee's type, that constraint should
    // include the call site's universe id, otherwise it would end up trying to
    // unify the callee's generic parameters without any artifact universe id.
    if let Some(own_universe_id) = &self.own_universe_id {
      assert!(!constraint_universe_stack.contains(&own_universe_id));
      constraint_universe_stack.push(own_universe_id.to_owned());
    }

    self.constraints.push((
      constraint_universe_stack,
      Constraint::Equality(ty, result.ty.clone()),
    ));

    let ty = result.ty.clone();

    self.extend(result);

    ty
  }

  pub(crate) fn infer_parameter(&mut self, parameter: &ast::Parameter) -> types::Type {
    let ty = if let Some(type_hint) = &parameter.type_hint {
      type_hint.to_owned()
    } else {
      // BUG: The inference system needs to be revised with regards to the constraints against generics; If a constraint set involving a generic and a type variable occurs, and the inference function was invoked by an artifact, the type variables might not end up becoming generics: they may ta ...
      // If the parameter has no type hint, its type will remain as a
      // type variable.
      self.create_type_variable("parameter")
    };

    // SAFETY: What if the type environment already contains an entry for the parameter's type id? Consider adding a catch-all wrapper function for inserting into the type environment, which would check for duplicates. Actually, this is possible if the same function is constrained more than once. What should be done to consider that fact? Of something being constrained/inferred more than once? Use caching via a catch-all `reference.infer`? Since all functions need to be referenced, for example? What about inline closures? They would technically be unique values, so no need for caching. This could be due to the same function, thus the same signature being called twice, and thus inferred twice+? If so, make a note of it.

    self.type_env.insert(parameter.type_id, ty.clone());

    ty
  }

  pub(crate) fn add_other_constraint(&mut self, constraint: Constraint) {
    let mut universe_stack = self.universe_stack.clone();

    // If the context's own constraint isn't considered, it would lead to a
    // situation like the following example:
    // 1. Call site inference context inherits from parent context.
    // 2. Universe stack contains parent universe id, not call site's.
    // 3. Any type on the call site's side is constrained against the callee's return type.
    // 4. The callee's return type is a generic.
    // 5. That constraint that was just created does NOT include the call site's universe id.
    // 6. During unification of such constraint, the universe id is missing from the constraint's universe stack.
    // 7. The generic cannot be resolved!
    if let Some(own_universe_id) = &self.own_universe_id {
      assert!(!universe_stack.contains(&own_universe_id));
      universe_stack.push(own_universe_id.to_owned());
    }

    self.constraints.push((universe_stack, constraint));
  }

  /// Create an equality constraint and add it to the constraint list,
  /// taking into account the current universe stack.
  pub(crate) fn add_constraint(&mut self, type_a: types::Type, type_b: types::Type) {
    self.add_other_constraint(Constraint::Equality(type_a, type_b))
  }

  pub(crate) fn finalize(self, ty: types::Type) -> InferenceResult {
    InferenceResult {
      constraints: self.constraints,
      universe_id: self.own_universe_id,
      type_var_substitutions: self.type_var_substitutions,
      type_env: self.type_env,
      id_count: self.id_generator.get_counter(),
      ty,
    }
  }

  fn extend(&mut self, other: InferenceResult) {
    // SAFETY: If it is valid/possible for the API to accept an 'older' context, then this assertion should be replaced with a `Result` type. Or if we're assuming that this would always be a logic bug, add a note. Also it is missing the reasoning message.
    assert!(other.id_count >= self.id_generator.get_counter());

    self.id_generator = auxiliary::IdGenerator::new(other.id_count);

    for (substitution_id, ty) in other.type_var_substitutions {
      assert!(!self.type_var_substitutions.contains_key(&substitution_id));
      self.type_var_substitutions.insert(substitution_id, ty);
    }

    for (type_id, ty) in other.type_env {
      // CONSIDER: Changing it so that instead of the type environment containing one type, it contains a set/vector of types, all of which should be compatible with one another (must be verified through unification). This is safer, because it ensures that any version of the same AST node with any input parameters, produces a compatible type.

      // TODO: If inference caching is added, add a check to ensure that no duplicates should ever be inserted into the type environment (assert that the current type environment doesn't contain the type id to be inserted). Also note that inference caching will need to consider polymorphic functions invoked from artifacts (in such cases, caching should not be used). But then, those polymorphic functions would be inserted multiple times onto the type environment...
      self.type_env.insert(type_id, ty.clone());
    }

    self.constraints.extend(other.constraints);
  }
}

// FIXME: 'Contamination' is a possible problem that needs to be addressed; contamination can occur when 'special' or 'unique' types are created that are supposed to be attached to specific AST nodes (ie. specific metadata in the type, or flags, or classification, etc.), but those types can be cloned and inserted as substitutions for type variables, thus associating the type unique with a different construct. This happens during unification. Some approaches that may be taken could possibly be extending the constraint enum to add an 'is_unique' flag, which should be respected during unification to prevent carbon cloning the type. One example of contamination would be the pointer type created for nullptr, as it has the special flag of 'is_nullptr', which allows an exception for the unification of pointer types against the opaque type. For such reason, it was decided not to special case for the nullptr, and instead force the user to use the opaque type hint for the null value instead.
#[derive(Clone, Debug)]
pub enum Constraint {
  /// Represents equality between two types.
  Equality(types::Type, types::Type),
  // CONSIDER: Another, perhaps more complex method would be to have tuples be similar to objects, but as a hash map. This way, it would have index -> element type mapping. It would need an open/closed system, similar to objects. Then, the 'element type of' can be modeled as an open tuple type, with key=index, and value=element type. This method of constraints might be more intuitive and simpler to manage, however.
  // CONSIDER: If this method works properly, replacing current object unification system with 'object element of' constraint.
  // REVIEW: If this occurs POST unification, then won't it unify against other things? In other words, it could only be a 'verification' constraint, since it won't aid unification?
  TupleElementOf {
    tuple_type: types::Type,
    element_type: types::Type,
    index: u32,
  },
}

pub(crate) trait Infer<'a> {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    // Default implementations to unit type.
    parent.inherit(None).finalize(types::Type::Unit)
  }
}

impl Infer<'_> for ast::Expr {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    match self {
      ast::Expr::Range(range) => parent.transient(range.as_ref()),
      ast::Expr::BinaryOp(binary_op) => parent.transient(binary_op.as_ref()),
      ast::Expr::CallSite(call_site) => parent.transient(call_site.as_ref()),
      ast::Expr::Cast(cast) => parent.transient(cast.as_ref()),
      ast::Expr::UnaryOp(unary_op) => parent.transient(unary_op.as_ref()),
      ast::Expr::Literal(literal) => parent.transient(literal),
      ast::Expr::Object(object) => parent.transient(object.as_ref()),
      ast::Expr::Unsafe(unsafe_) => parent.transient(unsafe_.as_ref()),
      ast::Expr::ObjectAccess(object_access) => parent.transient(object_access.as_ref()),
      ast::Expr::Tuple(tuple) => parent.transient(tuple.as_ref()),
      ast::Expr::TupleIndexing(tuple_indexing) => parent.transient(tuple_indexing.as_ref()),
      ast::Expr::Reference(reference) => parent.transient(reference.as_ref()),
      ast::Expr::Sizeof(sizeof) => parent.transient(sizeof.as_ref()),
      ast::Expr::Match(match_) => parent.transient(match_.as_ref()),
      ast::Expr::Group(group) => parent.transient(group.as_ref()),
      ast::Expr::Discard(discard) => parent.transient(discard.as_ref()),
      ast::Expr::PointerIndexing(pointer_indexing) => parent.transient(pointer_indexing.as_ref()),
      ast::Expr::Pass(..) => parent.inherit(None).finalize(types::Type::Unit),
      ast::Expr::If(if_) => parent.transient(if_.as_ref()),
      ast::Expr::Closure(closure) => parent.transient(closure.as_ref()),
      ast::Expr::Statement(statement) => parent.transient(statement.as_ref()),
      ast::Expr::UnionInstance(union_instance) => parent.transient(union_instance.as_ref()),
      ast::Expr::Block(block) => parent.transient(block.as_ref()),
      ast::Expr::With(with) => parent.transient(with.as_ref()),
      // TODO: Handle inference of effect constructs.
      ast::Expr::Try(try_) => todo!(),
      ast::Expr::Resume(resume) => todo!(),
    }
  }
}

impl Infer<'_> for ast::Item {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    match self {
      ast::Item::Binding(binding) => parent.transient(binding.as_ref()),
      ast::Item::ForeignVar(foreign_var) => parent.transient(foreign_var.as_ref()),
      ast::Item::Function(function) => parent.transient(function.as_ref()),
      ast::Item::Import(import) => parent.transient(import.as_ref()),
      ast::Item::Union(union) => parent.transient(union.as_ref()),
      ast::Item::UnionVariant(union_variant) => parent.transient(union_variant.as_ref()),
      ast::Item::TypeDef(type_def) => parent.transient(type_def.as_ref()),
      ast::Item::Constant(constant) => parent.transient(constant.as_ref()),
      ast::Item::ClosureCapture(closure_capture) => parent.transient(closure_capture.as_ref()),
      ast::Item::ForeignCluster(foreign_cluster) => parent.transient(foreign_cluster.as_ref()),
      ast::Item::ForeignFunction(foreign_function) => parent.transient(foreign_function.as_ref()),
      ast::Item::Parameter(parameter) => parent.transient(parameter.as_ref()),
      ast::Item::PointerAssignment(pointer_assignment) => {
        parent.transient(pointer_assignment.as_ref())
      }
      // TODO: Handle inference of effect constructs.
      ast::Item::Effect(effect) => todo!(),
    }
  }
}

impl Infer<'_> for ast::With {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let ty = context.visit(&self.object);

    // TODO: Constrain the deltas object to be a subtype of the object's type.
    todo!();

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Parameter {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let ty = context.infer_parameter(self);

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Import {
  //
}

impl Infer<'_> for ast::Union {
  //
}

impl Infer<'_> for ast::BinaryOp {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    let ty = match self.operator {
      ast::BinaryOperator::Add
      | ast::BinaryOperator::Subtract
      | ast::BinaryOperator::Multiply
      // NOTE: Although the logical approach would be to infer the
      // result of a division operation as a real number, prefer leaving
      // it as a type variable for greater flexibility. The result's type
      // will thus depend on the operands' types.
      | ast::BinaryOperator::Divide => context.create_type_variable("binary_op.arithmetic"),
      // TODO: The resulting type of modulo operations should be an integer, but with its bit-width corresponding with the bitwidth of the operands. Floats and integers alike should be allowed as operands. This will be a bit tricky, because those types cannot be inspected at this point (only post-unification are types revealed). Note that modulo operations can also result in negative integers. For now, `int64` is a good initial value because it encompasses all possible results (at the cost of possible redundancy).
      ast::BinaryOperator::Modulo => types::Type::Primitive(types::PrimitiveType::Integer(types::BitWidth::Width64, true)),
      ast::BinaryOperator::Equality
      | ast::BinaryOperator::Inequality
      | ast::BinaryOperator::And
      | ast::BinaryOperator::Or
      | ast::BinaryOperator::Nor
      | ast::BinaryOperator::GreaterThan
      | ast::BinaryOperator::GreaterThanOrEqual
      | ast::BinaryOperator::LessThan
      | ast::BinaryOperator::LessThanOrEqual
      | ast::BinaryOperator::Xor
      | ast::BinaryOperator::Nand => types::Type::Primitive(types::PrimitiveType::Bool),
      // TODO: Implement.
      ast::BinaryOperator::In => todo!(),
    };

    // TODO: Handle modulo operator.
    let operand_type = if let ast::BinaryOperator::Add
    | ast::BinaryOperator::Subtract
    | ast::BinaryOperator::Multiply
    | ast::BinaryOperator::Divide = self.operator
    {
      let operand_type = context.create_type_variable("binary_op.operand.numeric");

      context.add_constraint(operand_type.clone(), ty.clone());

      operand_type
    } else {
      context.create_type_variable("binary_op.operand")
    };

    context
      .type_env
      .insert(self.operand_type_id, operand_type.clone());

    context.constrain(&self.left_operand, operand_type.clone());
    context.constrain(&self.right_operand, operand_type.clone());
    context.type_env.insert(self.type_id, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::ForeignCluster {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    for foreign in &self.foreigns {
      context.visit(foreign);
    }

    context.finalize(types::Type::Unit)
  }
}

impl Infer<'_> for ast::ClosureCapture {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let ty = context.visit_target_via_link(&self.target_link_id).unwrap();

    context.type_env.insert(self.type_id, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Constant {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    context.constrain(self.value.as_ref(), self.ty.to_owned());

    context.finalize(self.ty.to_owned())
  }
}

impl Infer<'_> for ast::UnionVariant {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let context = parent.inherit(None);

    let union_item = context
      .symbol_table
      .registry
      .get(&self.union_id)
      .expect(auxiliary::BUG_NAME_RESOLUTION);

    let union = assert_extract!(union_item, symbol_table::RegistryItem::Union);

    context.finalize(types::Type::Union(std::rc::Rc::clone(union)))
  }
}

impl Infer<'_> for ast::PointerAssignment {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let pointee_type = context.create_type_variable("pointer_assignment.pointer.pointee");
    let pointer_type = types::Type::Pointer(Box::new(pointee_type.clone()));

    context.constrain(&self.pointer, pointer_type);
    context.constrain(&self.value, pointee_type);

    context.finalize(types::Type::Unit)
  }
}

impl Infer<'_> for ast::PointerIndexing {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let ty = context.visit(&self.pointer);

    context.type_env.insert(self.type_id, ty.clone());

    context.constrain(
      &self.index,
      types::Type::Primitive(types::PrimitiveType::Integer(
        types::BitWidth::Width64,
        false,
      )),
    );

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Discard {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    context.visit(&self.0);

    context.finalize(types::Type::Unit)
  }
}

impl Infer<'_> for ast::TupleIndex {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let tuple_type = context.create_type_variable("tuple.access");
    let element_type = context.create_type_variable("tuple.access.element");

    // BUG: (test:tuple_indexing_simple) This should be panicking with a `not yet implemented` message, since the unification's handling of `TupleElementOf` constraints is not yet implemented, but it's not panicking. Instead, unsolved type variable diagnostics are produced.
    context.add_other_constraint(Constraint::TupleElementOf {
      tuple_type: tuple_type.clone(),
      element_type: element_type.clone(),
      index: self.index,
    });

    context
      .type_env
      .insert(self.indexed_tuple_type_id, tuple_type.clone());

    context.type_env.insert(self.type_id, element_type.clone());
    context.constrain(&self.indexed_tuple, tuple_type);

    context.finalize(element_type)
  }
}

impl Infer<'_> for ast::UnionInstance {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let value_type = context.create_type_variable("union_instance.value");

    match &self.value {
      ast::UnionInstanceValue::Value(value) => {
        context.constrain(value, value_type);
      }
      ast::UnionInstanceValue::String(_) => context.add_constraint(
        value_type,
        types::Type::Primitive(types::PrimitiveType::CString),
      ),
      ast::UnionInstanceValue::Singleton(..) => context.add_constraint(
        value_type,
        types::Type::Primitive(types::PrimitiveType::Integer(
          types::BitWidth::Width64,
          false,
        )),
      ),
    };

    // BUG: Value type isn't constrained with anything for when the value is `String` or `Singleton` variant!
    todo!();

    let union_variant = assert_extract!(
      context
        .symbol_table
        .follow_link(&self.path.link_id)
        .expect(auxiliary::BUG_NAME_RESOLUTION),
      symbol_table::RegistryItem::UnionVariant
    );

    let union = assert_extract!(
      context
        .symbol_table
        .registry
        .get(&union_variant.union_id)
        .expect(auxiliary::BUG_NAME_RESOLUTION),
      symbol_table::RegistryItem::Union
    );

    context.finalize(types::Type::Union(std::rc::Rc::clone(union)))
  }
}

impl Infer<'_> for ast::Tuple {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    let element_types = self
      .elements
      .iter()
      .map(|element| context.visit(element))
      .collect();

    let ty = types::Type::Tuple(types::TupleType(element_types));

    context.type_env.insert(self.type_id, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::ForeignStatic {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    parent.inherit(None).finalize(self.ty.clone())
  }
}

impl Infer<'_> for ast::Range {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    parent
      .inherit(None)
      .finalize(types::Type::Range(self.start, self.end))
  }
}

impl Infer<'_> for ast::TypeDef {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    parent.inherit(None).finalize(self.body.to_owned())
  }
}

impl Infer<'_> for ast::Block {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    for statement in &self.statements {
      // Statement's types are irrelevant. However, they still need to be
      // visited. It should be noted that let-binding statements do have a
      // type themselves, but it is irrelevant in this context.
      context.visit(statement.as_ref());
    }

    let ty = context.visit(&self.yield_value);

    context.type_env.insert(self.type_id, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Statement {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    match self {
      ast::Statement::Binding(binding) => context.visit(binding.as_ref()),
      ast::Statement::Constant(constant) => context.visit(constant.as_ref()),
      ast::Statement::InlineExpr(expr) => context.visit(expr),
      ast::Statement::PointerAssignment(pointer_assignment) => {
        context.visit(pointer_assignment.as_ref())
      }
    };

    context.finalize(types::Type::Unit)
  }
}

impl Infer<'_> for ast::Function {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let signature_type = context.create_signature_type(&self.signature);

    // Cache the function type before inferring the body to allow
    // for recursion, otherwise they may try to retrieve the function type
    // when it hasn't been set yet.
    context
      .type_env
      .insert(self.type_id, types::Type::from(signature_type.clone()));

    context.constrain(
      self.body.as_ref(),
      signature_type.return_type.as_ref().clone(),
    );

    context.finalize(types::Type::from(signature_type))
  }
}

impl Infer<'_> for ast::Reference {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    let ty = context.visit_target_via_link(&self.path.link_id).unwrap();

    context.type_env.insert(self.type_id, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Literal {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    let ty = match &self.kind {
      ast::LiteralKind::Bool(_) => types::Type::Primitive(types::PrimitiveType::Bool),
      ast::LiteralKind::String(..) => types::Type::Primitive(types::PrimitiveType::CString),
      ast::LiteralKind::Char(..) => types::Type::Primitive(types::PrimitiveType::Char),
      ast::LiteralKind::Nullptr(type_hint) => {
        let ty = type_hint
          .as_ref()
          .map(|type_hint| type_hint.to_owned())
          .unwrap_or_else(|| context.create_type_variable("nullptr").into_pointer_type());

        ty
      }
      ast::LiteralKind::Number {
        bit_width,
        type_hint,
        is_real,
        ..
      } => {
        type_hint
          // OPTIMIZE: Cloning regardless.
          .to_owned()
          .map(|raw_type_hint| raw_type_hint)
          .unwrap_or(types::Type::Primitive(if *is_real {
            types::PrimitiveType::Real(bit_width.to_owned())
          } else {
            // Default to a signed integer type.
            types::PrimitiveType::Integer(bit_width.to_owned(), true)
          }))
      }
    };

    context.type_env.insert(self.type_id, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Cast {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let operand_type = context.visit(&self.operand);

    context
      .type_env
      .insert(self.operand_type_id, operand_type.clone());

    context
      .type_env
      .insert(self.type_id, self.cast_type.to_owned());

    context.finalize(self.cast_type.to_owned())
  }
}

impl Infer<'_> for ast::Binding {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    // TRACE: (test:vector_generics) Could it be that the bug related to the binding is due to the possibility that the value type here below is a generic type without any universe stack entry? It seems to be a type variable when printed to the console! Which may mean that it would be substituted to a generic type? If that's the case, that's a good indicator that the current inference system is quite fragile, especially around type variables, and the inference context and utility method logic needs to be more tightly isolated to prevent contamination or accidental logic bugs.
    let value_type = if let Some(type_hint) = &self.type_hint {
      context.constrain(&self.value, type_hint.to_owned())
    } else {
      context.visit(&self.value)
    };

    // Register the binding's type in the environment as that of its
    // value. This allows for references to attain the type of the binding's
    // value.
    context.type_env.insert(self.type_id, value_type.clone());

    // The binding's overall type is unit, since it is a statement. However,
    // references to the binding should have the type of the binding's value.
    context.finalize(value_type)
  }
}

impl Infer<'_> for ast::UnaryOp {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    let operand_type = match &self.operator {
      ast::UnaryOperator::Not => types::Type::Primitive(types::PrimitiveType::Bool),
      ast::UnaryOperator::Negate => context.create_type_variable("unary_op.ty"),
      ast::UnaryOperator::ReferenceOf => context.create_type_variable("unary_op.ref.operand"),
      ast::UnaryOperator::Dereference => context
        .create_type_variable("unary_op.deref.operand")
        .into_pointer_type(),
    };

    context
      .type_env
      .insert(self.operand_type_id, operand_type.clone());

    let ty = match &self.operator {
      ast::UnaryOperator::Not => types::Type::Primitive(types::PrimitiveType::Bool),
      ast::UnaryOperator::Negate => operand_type.clone(),
      ast::UnaryOperator::ReferenceOf => types::Type::Reference(Box::new(operand_type.clone())),
      ast::UnaryOperator::Dereference => match &operand_type {
        types::Type::Pointer(pointee) => pointee.as_ref().to_owned(),
        // REVISE: Attempt to revise the code to get rid of this assumption.
        _ => unreachable!("overall type should be a pointer"),
      },
    };

    // FIXME: This logic wrong. The type is already passed into the pointer creator on operand's type above, when the operator is a dereference. Something's wrong.
    // If the operator is a dereference, then the operand's type
    // must be a pointer, and the unary operation's type is the
    // pointee type.
    if let types::Type::Pointer(pointee_type) = &operand_type {
      context.add_constraint(ty.clone(), pointee_type.as_ref().clone());
    }

    context.type_env.insert(self.type_id, ty.clone());
    context.constrain(&self.operand, operand_type);

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::If {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    // Conditions must always be of type boolean.
    const CONDITION_TYPE: types::Type = types::Type::Primitive(types::PrimitiveType::Bool);

    let mut context = parent.inherit(None);

    context.constrain(&self.condition, CONDITION_TYPE);

    // FIXME: Need to slightly rework the type constraining process of the `if` statement. Currently, it is too monotone and restrictive. A field indicating whether the if produces a value or not is necessary. This is because different branches ARE allowed to have differing types, in the case that they don't yield a value, but instead currently it's forcing them to be `unit`.

    // The if expression will always have a unit type if it is missing
    // its else branch.
    let ty = if self.else_branch.is_none() {
      types::Type::Unit
    } else {
      context.create_type_variable("if")
    };

    context.type_env.insert(self.type_id, ty.clone());
    context.constrain(&self.then_branch, ty.clone());

    for (condition, alternative_branch) in &self.elif_branches {
      context.constrain(condition, CONDITION_TYPE);
      context.constrain(alternative_branch, ty.clone());
    }

    if let Some(else_value) = &self.else_branch {
      context.constrain(else_value, ty.clone());
    }

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Unsafe {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    parent.transient(&self.0)
  }
}

impl Infer<'_> for ast::CallSite {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    // TODO: If the callee is a generic function, and the amount of generic hints is LESS than the amount of generic parameters on the callee's generic object, then the remaining generic parameters should be inferred to type variables (to stay more idiomatic, pad the missing hints with `Infer`). Additionally, if any of the types are 'Infer`, then they should be substituted with fresh type variables (or should that occur during unification?). Actually, not precisely regarding the first point: generic hints must be provided ALL or NONE, if the user wants inference, THEY are forced to fill up the generic hints with `Infer` (by using '_'). In other words, under no circumstance should the amount of hints < the amount of generic parameters (unless they are not specified, in which case all the hints default to '_').

    // TODO: (test:generics_hints_mismatch) Need to constrain call site's generic hints vs. parameters (this may need to be done by first resolving the callee's signature, and then unifying (creating constraints) against it). Obviously, cannot resolve callee's signature at this point (during inference), so it would need to be some sort of deferred constraining (the usual: creating a signature type with type variables for the callee's signature, and constraining it against the call site's signature).

    // TODO: Handle variadic functions more explicitly and carefully here.

    // Only account universe stack if the call site is to a polymorphic callee,
    // otherwise it is not considered an artifact.
    let universe_id_opt = if !self.generic_hints.is_empty() {
      Some(self.universe_id.clone())
    } else {
      None
    };

    let mut context = parent.inherit(universe_id_opt);
    let return_type = context.create_type_variable("call_site.return");

    context.type_env.insert(self.type_id, return_type.clone());

    // BUG: The assumption that the callee is a callable will not always hold true by this point; unification hasn't yet occurred! This will panic if the callee is indeed not a callable, instead of being more graceful with a diagnostic.
    let callee = self.strip_callee(context.symbol_table).unwrap();

    let callee_arity_mode = context.determine_arity_mode_for_callable(&callee);

    let argument_types = self
      .arguments
      .iter()
      .map(|argument| {
        let ty = context.visit(&argument.value);

        context.type_env.insert(argument.type_id, ty.clone());

        ty
      })
      .collect::<Vec<_>>();

    // FIXME: The parameter types are being created as type variables, so that they make take the 'form' of generics. But! They are also being constrained against the argument types. So what happens if those type variables get unified against argument types BEFORE being unified against the generics?! Actually, the unification order shouldn't even matter! If they get unified against generics, they become generics, then unified against arguments, it's argument type vs. generic. If they are just a clone of the argument types, it's argument type vs. generic. In other words, nothing changes! Add a note here about this, so that the same mistake isn't made in the future thinking that parameter types need to be type variables to take the 'form' of generics.

    let callee_type = types::Type::Signature(types::SignatureType {
      parameter_types: argument_types,
      return_type: Box::new(return_type.clone()),
      arity_mode: callee_arity_mode,
    });

    context
      .type_env
      .insert(self.callee_type_id, callee_type.clone());

    context.constrain(&self.callee_expr, callee_type);

    // The type of the call expression is that of the callee's return
    // type.
    context.finalize(return_type)
  }
}

impl Infer<'_> for ast::ForeignFunction {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    for parameter in &self.signature.parameters {
      let parameter_type = parameter
        .type_hint
        .as_ref()
        .expect(auxiliary::BUG_FOREIGN_FN_TYPE_HINTS)
        .clone();

      context.type_env.insert(parameter.type_id, parameter_type);
    }

    let return_type = self
      .signature
      .return_type_hint
      .as_ref()
      .expect(auxiliary::BUG_FOREIGN_FN_TYPE_HINTS)
      .to_owned();

    let parameter_types = self
      .signature
      .parameters
      .iter()
      .map(|parameter| {
        parameter
          .type_hint
          .as_ref()
          .expect(auxiliary::BUG_FOREIGN_FN_TYPE_HINTS)
          .to_owned()
      })
      .collect();

    let arity_mode = if self.signature.is_variadic {
      types::ArityMode::Variadic {
        minimum_required_parameters: self.signature.parameters.len(),
      }
    } else {
      types::ArityMode::Fixed
    };

    let ty = types::Type::Signature(types::SignatureType {
      return_type: Box::new(return_type),
      arity_mode,
      parameter_types,
    });

    context.type_env.insert(self.type_id, ty.clone());

    // Allow for higher-order functions referencing the foreign function.
    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Sizeof {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    // NOTE: The type of the argument is irrelevant, since it is syntactically
    // guaranteed to be a type.

    let ty = types::Type::Primitive(types::PrimitiveType::Integer(
      types::BitWidth::Width64,
      false,
    ));

    let mut context = parent.inherit(None);

    context.type_env.insert(self.type_id, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::ObjectAccess {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let ty = context.create_type_variable("object_access.member");

    context.type_env.insert(self.type_id, ty.clone());

    // The base expression must be an object containing at least this field.
    let fields = types::ObjectFieldMap::from([(self.field_name.to_owned(), ty.clone())]);

    let base_type = types::Type::Object(types::ObjectType {
      fields,
      kind: types::ObjectKind::Open(context.id_generator.next_substitution_id()),
    });

    context.constrain(&self.object, base_type.clone());
    context.type_env.insert(self.base_expr_type_id, base_type);

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Closure {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let signature_type = context.create_signature_type(&self.signature);

    // Cache the function type before inferring the body to allow
    // for recursion, otherwise they may try to retrieve the function type
    // when it hasn't been set yet.
    context.type_env.insert(
      self.type_id,
      types::Type::from(signature_type.clone()).clone(),
    );

    for capture in &self.captures {
      context.visit(capture);
    }

    context.constrain(&self.body, signature_type.return_type.as_ref().clone());

    context.finalize(types::Type::from(signature_type))
  }
}

impl Infer<'_> for ast::Object {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);

    let fields = self
      .fields
      .iter()
      .map(|(name, field)| {
        // The object field's types are unknown.
        let field_type = context.create_type_variable("object.field");

        context.constrain(field, field_type.clone());

        (name.to_owned(), field_type)
      })
      .collect::<types::ObjectFieldMap>();

    let ty = types::Type::Object(types::ObjectType {
      fields,
      kind: types::ObjectKind::Closed,
    });

    context.type_env.insert(self.type_id, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Match {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    let mut context = parent.inherit(None);
    let ty = context.create_type_variable("match.value");
    let subject_type = context.visit(&self.subject);

    context
      .type_env
      .insert(self.subject_type_id, subject_type.clone());

    for arm in &self.arms {
      // All arm cases and bodies must be the same type.
      context.constrain(&arm.case, subject_type.clone());
      context.constrain(&arm.body, ty.clone());
    }

    context.type_env.insert(self.type_id, ty.clone());

    // The default case is always present. Use that to infer the
    // overall type of the match expression.
    context.constrain(&self.default_case, ty.clone());

    context.finalize(ty)
  }
}

impl Infer<'_> for ast::Group {
  fn infer(&self, parent: &InferenceContext<'_>) -> InferenceResult {
    parent.transient(&self.0)
  }
}
