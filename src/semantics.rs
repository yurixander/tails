use crate::{
  ast, auxiliary, diagnostic, force_extract, instantiation, lowering, resolution, symbol_table,
  types, visit,
};

pub struct SemanticCheckContext<'a> {
  universe_stack: resolution::UniverseStack,
  in_unsafe_scope: bool,
  current_function_id: Option<symbol_table::RegistryId>,
  symbol_table: &'a symbol_table::SymbolTable,
  function_id_stack: Vec<Option<symbol_table::RegistryId>>,
  resolution_helper: &'a resolution::ResolutionHelper<'a>,
  diagnostics: Vec<diagnostic::Diagnostic>,
}

impl<'a> SemanticCheckContext<'a> {
  pub(crate) fn new(
    symbol_table: &'a symbol_table::SymbolTable,
    resolution_helper: &'a resolution::ResolutionHelper<'_>,
  ) -> Self {
    Self {
      diagnostics: Vec::new(),
      symbol_table,
      in_unsafe_scope: false,
      current_function_id: None,
      universe_stack: resolution::UniverseStack::new(),
      function_id_stack: Vec::new(),
      resolution_helper,
    }
  }

  pub fn get_diagnostics(self) -> Vec<diagnostic::Diagnostic> {
    self.diagnostics
  }

  /// Determine whether this node is classified as a constant expression.
  ///
  /// For example, if this node is a literal value, or a group expression.
  /// Bindings will not be followed.
  pub fn is_constant(&self, expr: &ast::Expr) -> bool {
    // CONSIDER: Using indirect traversal to implement this check. If at any point a non-whitelisted ast item is encountered, then the item would not be considered constant.

    match expr.flatten() {
      // REVIEW: Should also ignore negation, since negation is normally applied to constants?
      ast::Expr::Pass(..) => true,
      ast::Expr::Literal(..) => true,
      // CONSIDER: Disallowing certain unary operators, such as "address of" and "dereference," since it doesn't make much sense to use as part of a constant, and could lead to problems.
      ast::Expr::UnaryOp(unary_expr) => self.is_constant(&unary_expr.operand),
      ast::Expr::Cast(cast) => self.is_constant(&cast.operand),
      ast::Expr::BinaryOp(binary_expr) => {
        self.is_constant(&binary_expr.left_operand) && self.is_constant(&binary_expr.right_operand)
      }
      ast::Expr::Tuple(tuple) => tuple
        .elements
        .iter()
        .all(|element| self.is_constant(element)),
      ast::Expr::Reference(reference) => {
        let registry_item = self
          .symbol_table
          .follow_link(&reference.path.link_id)
          .expect(auxiliary::BUG_NAME_RESOLUTION);

        if let symbol_table::RegistryItem::Constant(constant) = registry_item {
          return self.is_constant(&constant.value);
        }

        false
      }
      _ => false,
    }
  }

  /// Determines if the given node requires an unsafe context to be executed.
  ///
  /// This function checks if a specific node, such as a unary operation with
  /// dereferencing, pointer assignment, pointer indexing, or foreign function calls,
  /// requires an unsafe context. If the node requires an unsafe context, the function
  /// returns `Some(true)`. If the node does not require an unsafe context, the function
  /// returns `Some(false)`. If the node's safety requirement cannot be determined, the
  /// function returns `None`.
  ///
  /// This function does not take into account nested subnodes that may require unsafe context,
  /// only the given node directly.
  pub fn requires_unsafe_context(&self, expr: &ast::Expr) -> Option<bool> {
    match expr {
      ast::Expr::PointerAssignment(_) | ast::Expr::PointerIndexing(_) => Some(true),
      ast::Expr::UnaryOp(unary_op) if unary_op.operator == ast::UnaryOperator::Dereference => {
        Some(true)
      }
      ast::Expr::CallSite(call_site) => {
        let callee = call_site.strip_callee(self.symbol_table).unwrap();

        Some(matches!(callee, ast::Callable::ForeignFunction(_)))
      }
      _ => Some(false),
    }
  }

  fn push_function_id(&mut self) {
    self.function_id_stack.push(self.current_function_id);
  }

  fn pop_function_id(&mut self) {
    self.current_function_id = self.function_id_stack.pop().flatten();
  }
}

impl<'a> visit::ArtifactVisitor for SemanticCheckContext<'a> {
  fn get_universe_stack(&self) -> &resolution::UniverseStack {
    &self.universe_stack
  }

  fn push_universe_id(&mut self, universe_id: symbol_table::UniverseId) {
    assert!(!self.universe_stack.contains(&universe_id));
    self.universe_stack.push(universe_id);
  }

  fn set_universe_stack(&mut self, universe_stack: resolution::UniverseStack) {
    self.universe_stack = universe_stack;
  }
}

impl<'a> visit::Visitor for SemanticCheckContext<'a> {
  fn default_value(&mut self) {
    //
  }

  fn exit_item(&mut self, item: &ast::Item) {
    match item {
      ast::Item::Function(_) => {
        self.current_function_id = None;
      }
      _ => {}
    };
  }

  fn enter_expr(&mut self, expr: &ast::Expr) {
    // BUG: (test:object) Statements nor call sites are 'entered'. The problem seems to be caused by the fact that analysis passes visit all top-level items without considering whether they are polymorphic or not. When polymorphic functions are traversed, they have no precursor artifact id buffer set (from call sites). Then, when polymorphic types within these functions are attempted to be resolved, since no context artifact id is given, the process fails. This failure may occur specifically before even the body of the polymorphic function is visited, perhaps in the `visit_function` function, when resolving the function's signature type.
    if self
      .requires_unsafe_context(expr)
      .expect(auxiliary::BUG_NAME_RESOLUTION)
      && !self.in_unsafe_scope
    {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::CannotUseOutsideUnsafe);
    }
  }

  fn exit_expr(&mut self, expr: &ast::Expr) {
    match expr {
      ast::Expr::Closure(_) => {
        self.pop_function_id();
      }
      ast::Expr::Unsafe(_) => {
        // TODO: Might need to use a stack or buffer because of the possibility of nested unsafe expressions? Might also consider to disallow nested unsafe exprs entirely via errors.
        self.in_unsafe_scope = false;
      }
      _ => {}
    }
  }

  fn visit_type_def(&mut self, type_def: &ast::TypeDef) {
    if type_def
      .body
      .contains_directly_recursive_types(self.symbol_table)
      .expect(auxiliary::BUG_NAME_RESOLUTION)
    {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::RecursiveType(type_def.body.clone()))
    }
  }

  fn visit_function(&mut self, function: &ast::Function) {
    self.current_function_id = Some(function.registry_id);

    if function.is_polymorphic() {
      assert!(
        !self.universe_stack.is_empty(),
        "polymorphic functions should not be visited with an empty universe stack (universe stack not updated when visiting a polymorphic item?)"
      );
    }

    if function.signature.is_variadic {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::FunctionsCannotBeVariadic(
          function.name.to_owned(),
        ));
    }

    let signature_type = self
      .resolution_helper
      // TRACE: (test:generics_call_chain) THIS is the original call that leads to the fault. It's strangely from the semantic check pass? Could it be that wrong tracking of universe stack on this pass is what leads to the fault, instead of the problem being with inference/instantiation itself?
      .resolve_by_id(&function.type_id, self.universe_stack.clone())
      .expect(auxiliary::BUG_MISSING_TYPE);

    if function.name == lowering::ENTRY_POINT_NAME {
      let main_function_signature = types::Type::Signature(types::SignatureType {
        parameter_types: vec![
          types::Type::Primitive(types::PrimitiveType::Integer(
            types::BitWidth::Width32,
            false,
          )),
          types::Type::Primitive(types::PrimitiveType::CString).into_pointer_type(),
        ],
        return_type: Box::new(types::Type::Primitive(types::PrimitiveType::Integer(
          types::BitWidth::Width32,
          false,
        ))),
        arity_mode: types::ArityMode::Fixed,
      });

      if !instantiation::InstantiationHelper::compare_by_unification(
        // OPTIMIZE: Avoid cloning. This should be optimized on the `compare_by_unification` function, not here (as it is enforced by the function).
        signature_type.into_owned(),
        main_function_signature,
        self.symbol_table,
      ) {
        self
          .diagnostics
          .push(diagnostic::Diagnostic::MainFunctionSignatureMismatch);
      }
    }
  }

  fn visit_constant(&mut self, constant: &ast::Constant) {
    // TODO: Must check that no division by zero is performed. The denominator *can* be extracted IF the constant's value is indeed constant.

    if !self.is_constant(&constant.value) {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::ConstantValueNotConstant);
    }
  }

  fn visit_statement(&mut self, statement: &ast::Statement) {
    if let ast::Statement::InlineExpr(inner_expr) = statement {
      let type_id = inner_expr.find_type_id();

      // If the statement's inner item does not have a type id, then assume
      // its type to be unit.
      let ty = type_id
        .map(|type_id| {
          self
            .resolution_helper
            .resolve_by_id(&type_id, self.universe_stack.clone())
            .expect(auxiliary::BUG_MISSING_TYPE)
            // OPTIMIZE: Any way to avoid cloning?
            .into_owned()
        })
        .unwrap_or(types::Type::Unit);

      if !ty.is_a_unit() {
        self
          .diagnostics
          .push(diagnostic::Diagnostic::UnusedValueMustBeUsedOrDiscarded);
      }
    }
  }

  fn visit_unsafe(&mut self, unsafe_: &ast::Unsafe) {
    if self.in_unsafe_scope {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::NestedUnsafeScopes);
    }

    if self.is_constant(&unsafe_.0) {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::ConditionOrValueIsConstant);
    }

    // CONSIDER: To avoid problems with nested cases, save a buffer here, then restore? Maybe there's no need to restore the flag with its previous buffer in this specific case.
    // TODO: Add a test case to see if a case with nested unsafe blocks would cause problems.
    self.in_unsafe_scope = true;
  }

  fn visit_if(&mut self, if_: &ast::If) {
    if self.is_constant(&if_.condition) {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::ConditionOrValueIsConstant);
    }
  }

  fn visit_call_site(&mut self, call_site: &ast::CallSite) {
    let callee = call_site.strip_callee(self.symbol_table).unwrap();

    // REVIEW: Shouldn't this be handled implicitly by the type unification algorithm?
    if let ast::Callable::Function(function) = callee {
      if !function.is_polymorphic() && !call_site.generic_hints.is_empty() {
        self
          .diagnostics
          .push(diagnostic::Diagnostic::FunctionTakesNoGenericParameters(
            function.name.to_owned(),
          ));
      }
      // Since only artifacts (call sites with generic hints) are collected
      // and processed during instantiation, the instantiation phase will not
      // report diagnostics for missing generic hints. Thus, it must be checked
      // here.
      else if function.is_polymorphic() && call_site.generic_hints.is_empty() {
        self
          .diagnostics
          .push(diagnostic::Diagnostic::FunctionMissingGenericHints(
            function.name.to_owned(),
          ));
      }
    }
  }

  fn visit_closure(&mut self, closure: &ast::Closure) {
    self.push_function_id();
    self.current_function_id = Some(closure.registry_id);
  }

  fn visit_range(&mut self, range: &ast::Range) {
    // NOTE: No need to check whether the range's bounds are constant
    // expressions, as it is syntactically guaranteed by the parser.

    // SAFETY: What if they're equal?
    if range.start > range.end {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::RangeStartMustBeLessOrEqualToEnd(
          range.start,
          range.end,
        ));
    }
  }

  fn visit_cast(&mut self, cast: &ast::Cast) {
    let operand_type = self
      .resolution_helper
      .resolve_by_id(&cast.operand_type_id, self.universe_stack.clone())
      .expect(auxiliary::BUG_MISSING_TYPE);

    let cast_type = self
      .resolution_helper
      .resolve_by_id(&cast.type_id, self.universe_stack.clone())
      .expect(auxiliary::BUG_MISSING_TYPE);

    fn is_valid_cast_type(ty: &types::Type) -> bool {
      matches!(
        ty,
        types::Type::Pointer(..) | types::Type::Primitive(..) | types::Type::Opaque
      )
    }

    if !is_valid_cast_type(&operand_type) || !is_valid_cast_type(&cast_type) {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::InvalidCastType);
    }

    // Cast between pointer types must occur within an unsafe
    // scope.
    if matches!(operand_type.as_ref(), types::Type::Pointer(..))
      || matches!(cast_type.as_ref(), types::Type::Pointer(..)) && !self.in_unsafe_scope
    {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::CannotUseOutsideUnsafe);
    }

    // TODO: Check if the cast and operand types are the same, thus making the operation redundant. Type equality comparison must be done through unification.
  }

  fn visit_match(&mut self, match_: &ast::Match) {
    if self.is_constant(&match_.subject) {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::ConditionOrValueIsConstant);
    }
  }

  fn visit_tuple_indexing(&mut self, tuple_indexing: &ast::TupleIndex) {
    let tuple_general_type = self
      .resolution_helper
      .resolve_by_id(
        &tuple_indexing.indexed_tuple_type_id,
        self.universe_stack.clone(),
      )
      .expect(auxiliary::BUG_MISSING_TYPE);

    let tuple_type = force_extract!(tuple_general_type.as_ref(), types::Type::Tuple);

    if tuple_indexing.index as usize >= tuple_type.0.len() {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::TupleAccessOutOfBounds {
          index: tuple_indexing.index as usize,
          tuple_length: tuple_type.0.len(),
        });
    }
  }
}
