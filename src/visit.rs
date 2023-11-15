use crate::{ast, instantiation, resolution, symbol_table, types};

macro_rules! define_visit_fn {
  ($method_name:ident, $type_name:ty) => {
    fn $method_name(&mut self, _item: &$type_name) -> T {
      self.default_value()
    }
  };
}

pub(crate) trait ArtifactVisitor {
  fn get_universe_stack(&self) -> &resolution::UniverseStack;

  fn set_universe_stack(&mut self, universe_stack: resolution::UniverseStack);

  fn push_universe_id(&mut self, universe_id: symbol_table::UniverseId);
}

pub(crate) fn traverse_possibly_polymorphic_global_item(
  global_item: &ast::Item,
  reverse_context_artifact_id_tracker: &instantiation::ReverseUniverseTracker,
  context: &mut (impl ArtifactVisitor + Visitor),
) {
  // In case that the item is polymorphic, but has no corresponding
  // context artifact ids, do not traverse it with an empty universe stack.
  if !global_item.is_polymorphic() {
    global_item.traverse(context);

    return;
  }

  let registry_id = global_item
    .find_registry_id()
    .expect("polymorphic items should always have corresponding declaration ids");

  // It is valid and acceptable for polymorphic items to have no instantiations,
  // and thus no corresponding context artifact ids registered. For example, a
  // polymorphic function might not necessarily be called anywhere.
  if let Some(artifact_ids) = reverse_context_artifact_id_tracker.get(registry_id) {
    // BUG: Awaiting fix of logic bug that causes certain polymorphic items to be visited with incomplete universe stacks. For example, in a generic call chain, since call site artifacts are added to the reverse universe tracker without knowledge of THEIR previous call sites (call chain), if they pass generic hints that are generic types, those generic types cannot be resolved. This requires a slightly different perspective in terms of the implementation of traversing polymorphic items with their artifacts: the regular stack-based, push-pop artifact approach, instead of the current 'collect all artifacts and for each, visit', the difference being that one considers context (stack-based) and the other does not.
    // traverse_polymorphic_item(global_item, artifact_ids, context);
  }
}

pub(crate) fn traverse_polymorphic_item(
  item: &ast::Item,
  with_universe_stack: &[symbol_table::UniverseId],
  artifact_visitor: &mut (impl ArtifactVisitor + Visitor),
) {
  // REVIEW: Is there a need to get the current universe stack, since won't it be visiting global items, so the universe stack SHOULD always be empty?
  let previous_universe_stack = artifact_visitor.get_universe_stack().to_owned();

  for artifact_id in with_universe_stack {
    artifact_visitor.push_universe_id(artifact_id.to_owned());
    item.traverse(artifact_visitor);
  }

  artifact_visitor.set_universe_stack(previous_universe_stack);
}

pub trait Visitor<T = ()> {
  fn default_value(&mut self) -> T;

  fn visit_item(&mut self, item: &ast::Item) -> T
  where
    Self: Sized,
  {
    self.enter_item(item);

    let result = item.accept(self);

    self.exit_item(item);

    result
  }

  fn visit_expr(&mut self, expr: &ast::Expr) -> T
  where
    Self: Sized,
  {
    self.enter_expr(expr);

    let result = expr.accept(self);

    self.exit_expr(expr);

    result
  }

  // FIXME: Some nodes will never reach this point, namely those that aren't in the form of `ast::Node` in the AST.
  // CONSIDER: Getting rid of this method, since the `visit` method will always be called and can be used instead. This method just adds complexity for a slight organizational benefit.
  fn enter_item(&mut self, _item: &ast::Item) {
    //
  }

  fn exit_item(&mut self, _item: &ast::Item) {
    //
  }

  fn enter_expr(&mut self, _expr: &ast::Expr) {
    //
  }

  fn exit_expr(&mut self, _expr: &ast::Expr) {
    //
  }

  define_visit_fn!(visit_module, ast::Module);
  define_visit_fn!(visit_pass, ast::Pass);
  define_visit_fn!(visit_function, ast::Function);
  define_visit_fn!(visit_literal, ast::Literal);
  define_visit_fn!(visit_unsafe, ast::Unsafe);
  define_visit_fn!(visit_discard, ast::Discard);
  define_visit_fn!(visit_foreign_cluster, ast::ForeignCluster);
  define_visit_fn!(visit_foreign_function, ast::ForeignFunction);
  define_visit_fn!(visit_foreign_var, ast::ForeignStatic);
  define_visit_fn!(visit_block, ast::Block);
  define_visit_fn!(visit_binding, ast::Binding);
  define_visit_fn!(visit_if, ast::If);
  define_visit_fn!(visit_call_site, ast::CallSite);
  define_visit_fn!(visit_statement, ast::Statement);
  define_visit_fn!(visit_reference, ast::Reference);
  define_visit_fn!(visit_binary_op, ast::BinaryOp);
  define_visit_fn!(visit_unary_op, ast::UnaryOp);
  define_visit_fn!(visit_parameter, ast::Parameter);
  define_visit_fn!(visit_signature, ast::Signature);
  define_visit_fn!(visit_object, ast::Object);
  define_visit_fn!(visit_path, ast::Path);
  define_visit_fn!(visit_type_def, ast::TypeDef);
  define_visit_fn!(visit_closure, ast::Closure);
  define_visit_fn!(visit_object_access, ast::ObjectAccess);
  define_visit_fn!(visit_group, ast::Group);
  define_visit_fn!(visit_import, ast::Import);
  define_visit_fn!(visit_sizeof, ast::Sizeof);
  define_visit_fn!(visit_type, types::Type);
  define_visit_fn!(visit_cast, ast::Cast);
  define_visit_fn!(visit_match, ast::Match);
  define_visit_fn!(visit_tuple, ast::Tuple);
  define_visit_fn!(visit_union, ast::Union);
  define_visit_fn!(visit_union_instance, ast::UnionInstance);
  define_visit_fn!(visit_union_variant, ast::UnionVariant);
  define_visit_fn!(visit_tuple_indexing, ast::TupleIndex);
  define_visit_fn!(visit_effect, ast::Effect);
  define_visit_fn!(visit_try, ast::Try);
  define_visit_fn!(visit_resume, ast::Resume);
  define_visit_fn!(visit_pointer_indexing, ast::PointerIndexing);
  define_visit_fn!(visit_pointer_assignment, ast::PointerAssignment);
  define_visit_fn!(visit_constant, ast::Constant);
  define_visit_fn!(visit_closure_capture, ast::ClosureCapture);
  define_visit_fn!(visit_with, ast::With);
}

// CONSIDER: Extending with consideration for the `enter_item` and `exit_item` functions.
pub trait Visitable {
  /// Visit the item itself only, but not its children.
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T;

  /// Visit all the item's children (if any), but not the item itself.
  fn traverse_children<T>(&self, _visitor: &mut dyn Visitor<T>) {
    //
  }

  /// Visit both the item and its children recursively.
  fn traverse<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.accept(visitor);
    self.traverse_children(visitor);
  }
}

#[inline]
fn traverse_many<T>(items: &[impl Visitable], visitor: &mut dyn Visitor<T>) {
  for item in items {
    item.traverse(visitor);
  }
}

impl Visitable for ast::Item {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    match self {
      ast::Item::Binding(binding) => binding.accept(visitor),
      ast::Item::Constant(constant) => constant.accept(visitor),
      ast::Item::Function(function) => function.accept(visitor),
      ast::Item::ForeignVar(foreign_var) => foreign_var.accept(visitor),
      ast::Item::ForeignCluster(foreign_cluster) => foreign_cluster.accept(visitor),
      ast::Item::Parameter(parameter) => parameter.accept(visitor),
      ast::Item::TypeDef(type_def) => type_def.accept(visitor),
      ast::Item::Union(union) => union.accept(visitor),
      ast::Item::UnionVariant(union_variant) => union_variant.accept(visitor),
      ast::Item::ClosureCapture(closure_capture) => closure_capture.accept(visitor),
      ast::Item::Import(import) => import.accept(visitor),
      ast::Item::Effect(effect) => effect.accept(visitor),
      ast::Item::ForeignFunction(foreign_function) => foreign_function.accept(visitor),
      ast::Item::PointerAssignment(pointer_assignment) => pointer_assignment.accept(visitor),
    }
  }

  fn traverse<T>(&self, visitor: &mut dyn Visitor<T>) {
    // TRACE: (test:object) Items inside `Statements` do not seem trigger/invoke `enter_item` and `exit_item` (instead the statement itself is only visited?). Actually, not even statements are 'entered'.
    visitor.enter_item(&self);
    self.accept(visitor);
    self.traverse_children(visitor);
    visitor.exit_item(self);
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    // BUG: [!] A lot of nodes are directly visited, but not passed through `enter_node` and `exit_node` (for example, paths).

    match self {
      ast::Item::Binding(binding) => binding.traverse_children(visitor),
      ast::Item::Constant(constant) => constant.traverse_children(visitor),
      ast::Item::Function(function) => function.traverse_children(visitor),
      ast::Item::ForeignFunction(foreign_function) => foreign_function.traverse_children(visitor),
      ast::Item::ForeignVar(foreign_var) => foreign_var.traverse_children(visitor),
      ast::Item::ForeignCluster(foreign_cluster) => foreign_cluster.traverse_children(visitor),
      ast::Item::Parameter(parameter) => parameter.traverse_children(visitor),
      ast::Item::TypeDef(type_def) => type_def.traverse_children(visitor),
      ast::Item::Union(union) => union.traverse_children(visitor),
      ast::Item::UnionVariant(union_variant) => union_variant.traverse_children(visitor),
      ast::Item::ClosureCapture(closure_capture) => closure_capture.traverse_children(visitor),
      ast::Item::Import(import) => import.traverse_children(visitor),
      ast::Item::Effect(effect) => effect.traverse_children(visitor),
      ast::Item::PointerAssignment(pointer_assignment) => {
        pointer_assignment.traverse_children(visitor)
      }
    }
  }
}

impl Visitable for ast::Expr {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    match self {
      ast::Expr::BinaryOp(binary_op) => binary_op.accept(visitor),
      ast::Expr::UnaryOp(unary_op) => unary_op.accept(visitor),
      ast::Expr::Literal(literal) => literal.accept(visitor),
      ast::Expr::Reference(reference) => reference.accept(visitor),
      ast::Expr::Unsafe(unsafe_) => unsafe_.accept(visitor),
      ast::Expr::Cast(cast) => cast.accept(visitor),
      ast::Expr::Closure(closure) => closure.accept(visitor),
      ast::Expr::CallSite(call_site) => call_site.accept(visitor),
      ast::Expr::Discard(discard) => discard.accept(visitor),
      ast::Expr::Group(group) => group.accept(visitor),
      ast::Expr::Object(object) => object.accept(visitor),
      ast::Expr::If(if_) => if_.accept(visitor),
      ast::Expr::Try(try_) => try_.accept(visitor),
      ast::Expr::Match(match_) => match_.accept(visitor),
      ast::Expr::Block(block) => block.accept(visitor),
      ast::Expr::Resume(resume) => resume.accept(visitor),
      ast::Expr::Sizeof(size_of) => size_of.accept(visitor),
      ast::Expr::PointerIndexing(pointer_indexing) => pointer_indexing.accept(visitor),
      ast::Expr::Tuple(tuple) => tuple.accept(visitor),
      ast::Expr::ObjectAccess(object_access) => object_access.accept(visitor),
      ast::Expr::TupleIndexing(tuple_indexing) => tuple_indexing.accept(visitor),
      ast::Expr::Pass(nothing) => nothing.accept(visitor),
      ast::Expr::Statement(statement) => statement.accept(visitor),
      ast::Expr::UnionInstance(union_instance) => union_instance.accept(visitor),
      ast::Expr::With(with) => with.accept(visitor),
    }
  }

  fn traverse<T>(&self, visitor: &mut dyn Visitor<T>) {
    visitor.enter_expr(self);
    self.accept(visitor);
    self.traverse_children(visitor);
    visitor.exit_expr(self);
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    match self {
      ast::Expr::BinaryOp(binary_op) => binary_op.traverse_children(visitor),
      ast::Expr::UnaryOp(unary_op) => unary_op.traverse_children(visitor),
      ast::Expr::Literal(literal) => literal.traverse_children(visitor),
      ast::Expr::Reference(reference) => reference.traverse_children(visitor),
      ast::Expr::Unsafe(unsafe_) => unsafe_.traverse_children(visitor),
      ast::Expr::Cast(cast) => cast.traverse_children(visitor),
      ast::Expr::Closure(closure) => closure.traverse_children(visitor),
      ast::Expr::CallSite(call_site) => call_site.traverse_children(visitor),
      ast::Expr::Discard(discard) => discard.traverse_children(visitor),
      ast::Expr::Group(group) => group.traverse_children(visitor),
      ast::Expr::Object(object) => object.traverse_children(visitor),
      ast::Expr::If(if_) => if_.traverse_children(visitor),
      ast::Expr::Try(try_) => try_.traverse_children(visitor),
      ast::Expr::Match(match_) => match_.traverse_children(visitor),
      ast::Expr::Block(block) => block.traverse_children(visitor),
      ast::Expr::Resume(resume) => resume.traverse_children(visitor),
      ast::Expr::Sizeof(size_of) => size_of.traverse_children(visitor),
      ast::Expr::PointerIndexing(pointer_indexing) => pointer_indexing.traverse_children(visitor),
      ast::Expr::Tuple(tuple) => tuple.traverse_children(visitor),
      ast::Expr::ObjectAccess(object_access) => object_access.traverse_children(visitor),
      ast::Expr::TupleIndexing(tuple_indexing) => tuple_indexing.traverse_children(visitor),
      ast::Expr::Pass(nothing) => nothing.traverse_children(visitor),
      ast::Expr::Statement(statement) => statement.traverse_children(visitor),
      ast::Expr::UnionInstance(union_instance) => union_instance.traverse_children(visitor),
      ast::Expr::With(with) => with.traverse_children(visitor),
    }
  }
}

impl Visitable for ast::With {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_with(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.object.traverse(visitor);
    self.deltas.traverse(visitor);
  }
}

impl Visitable for ast::ForeignCluster {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_foreign_cluster(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    for foreign in &self.foreigns {
      foreign.traverse(visitor);
    }
  }
}

impl Visitable for ast::Pass {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_pass(self)
  }
}

impl Visitable for ast::Discard {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_discard(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.0.traverse(visitor);
  }
}

impl Visitable for ast::Literal {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_literal(self)
  }
}

impl Visitable for ast::ClosureCapture {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_closure_capture(self)
  }
}

impl Visitable for ast::Effect {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_effect(self)
  }
}

impl Visitable for ast::ForeignStatic {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_foreign_var(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.ty.traverse(visitor);
  }
}

impl Visitable for ast::Group {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_group(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.0.traverse(visitor);
  }
}

impl Visitable for ast::Import {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_import(self)
  }
}

impl Visitable for ast::Statement {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_statement(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    let inner_value_as_item = match self {
      ast::Statement::InlineExpr(inline_expr) => {
        inline_expr.traverse(visitor);

        return;
      }
      ast::Statement::Binding(binding) => ast::Item::Binding(binding.clone()),
      ast::Statement::Constant(constant) => ast::Item::Constant(constant.clone()),
      ast::Statement::PointerAssignment(pointer_assignment) => {
        ast::Item::PointerAssignment(pointer_assignment.clone())
      }
    };

    inner_value_as_item.traverse(visitor);
  }
}

impl Visitable for ast::ObjectAccess {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_object_access(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.object.traverse(visitor);
  }
}

impl Visitable for ast::Resume {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_resume(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.condition.traverse(visitor);
  }
}

impl Visitable for ast::TupleIndex {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_tuple_indexing(self)
  }
}

impl Visitable for ast::UnaryOp {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_unary_op(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.operand.traverse(visitor);
  }
}

impl Visitable for ast::Path {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_path(self)
  }
}

impl Visitable for ast::Reference {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_reference(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.path.traverse(visitor);
  }
}

impl Visitable for ast::Try {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_try(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.expr.traverse(visitor);
    // visitor.visit_effect(&try_.handlers);

    // TODO: Visit effect handlers, and default effect handler.
    todo!();
  }
}

impl Visitable for ast::Unsafe {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_unsafe(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.0.traverse(visitor);
  }
}

impl Visitable for ast::Sizeof {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_sizeof(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.ty.traverse(visitor);
  }
}

impl Visitable for ast::PointerAssignment {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_pointer_assignment(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.pointer.traverse(visitor);
    self.value.traverse(visitor);
  }
}

impl Visitable for ast::ForeignFunction {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_foreign_function(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.signature.traverse(visitor)
  }
}

impl Visitable for ast::Cast {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_cast(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.operand.traverse(visitor);
    self.cast_type.traverse(visitor);
  }
}

impl Visitable for ast::Parameter {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_parameter(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    if let Some(type_hint) = &self.type_hint {
      type_hint.traverse(visitor);
    }
  }
}

impl Visitable for ast::Tuple {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_tuple(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    traverse_many(&self.elements, visitor);
  }
}

impl Visitable for ast::PointerIndexing {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_pointer_indexing(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.pointer.traverse(visitor);
    self.index.traverse(visitor);
  }
}

impl Visitable for ast::CallSite {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_call_site(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.callee_expr.traverse(visitor);

    for argument in &self.arguments {
      argument.value.traverse(visitor);
    }

    for hint in &self.generic_hints {
      hint.traverse(visitor);
    }
  }
}

impl Visitable for ast::BinaryOp {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_binary_op(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.left_operand.traverse(visitor);
    self.right_operand.traverse(visitor);
  }
}

impl Visitable for ast::Object {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_object(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    for field in &self.fields {
      field.1.traverse(visitor);
    }
  }
}

impl Visitable for ast::Binding {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_binding(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    if let Some(type_hint) = &self.type_hint {
      type_hint.traverse(visitor);
    }

    self.value.traverse(visitor);
  }
}

impl Visitable for ast::TypeDef {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_type_def(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.generics.traverse(visitor);
    self.body.traverse(visitor)
  }
}

impl Visitable for ast::Match {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_match(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.subject.traverse(visitor);

    for arm in &self.arms {
      arm.case.traverse(visitor);
      arm.body.traverse(visitor);
    }

    self.default_case.traverse(visitor);
  }
}

impl Visitable for ast::If {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_if(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.condition.traverse(visitor);
    self.then_branch.traverse(visitor);

    for alternative_branches in &self.elif_branches {
      alternative_branches.0.traverse(visitor);
      alternative_branches.1.traverse(visitor);
    }

    if let Some(else_expr) = &self.else_branch {
      else_expr.traverse(visitor);
    }
  }
}

impl Visitable for ast::Union {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_union(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    for variant in &self.variants {
      // REVIEW: Why clone RC here? Because it becomes an `Item`, and that allows for the `enter_item` and `exit_item` functions to be called?
      ast::Item::UnionVariant(variant.1.clone()).traverse(visitor);
    }
  }
}

impl Visitable for ast::UnionInstance {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_union_instance(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.path.traverse(visitor);

    match &self.value {
      ast::UnionInstanceValue::Value(value) => {
        value.traverse(visitor);
      }
      ast::UnionInstanceValue::Singleton(_) | ast::UnionInstanceValue::String(_) => {}
    }
  }
}

impl Visitable for ast::UnionVariant {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_union_variant(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    match &self.kind {
      ast::UnionVariantKind::Type(ty) => {
        ty.traverse(visitor);
      }
      _ => {}
    }
  }
}

impl Visitable for ast::Constant {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_constant(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.ty.traverse(visitor);
    self.value.traverse(visitor);
  }
}

impl Visitable for ast::Closure {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_closure(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    for capture in &self.captures {
      // FIXME: Awaiting consistent logic for `enter` and `exit` functions for non-items.
      visitor.enter_item(&ast::Item::ClosureCapture(std::rc::Rc::new(
        capture.to_owned(),
      )));

      visitor.visit_closure_capture(capture);
    }

    self.signature.traverse(visitor);
    self.body.traverse(visitor);
  }
}

impl Visitable for ast::Block {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_block(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    for statement in &self.statements {
      statement.traverse(visitor);
    }

    self.yield_value.traverse(visitor);
  }
}

impl Visitable for ast::Signature {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_signature(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    for parameter in &self.parameters {
      if let Some(type_hint) = &parameter.type_hint {
        type_hint.traverse(visitor);
      }

      parameter.traverse(visitor);
    }

    if let Some(return_type_hint) = &self.return_type_hint {
      return_type_hint.traverse(visitor);
    }
  }
}

impl Visitable for ast::Function {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_function(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    self.generics.traverse(visitor);
    self.signature.traverse(visitor);
    self.body.traverse(visitor);
  }
}

impl Visitable for types::Type {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.visit_type(self)
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    match self {
      types::Type::Signature(signature_type) => {
        for parameter_type in &signature_type.parameter_types {
          parameter_type.traverse(visitor);
        }

        signature_type.return_type.traverse(visitor);
      }
      types::Type::Stub(stub_type) => {
        stub_type.path.traverse(visitor);

        for generic_hint in &stub_type.generic_hints {
          generic_hint.traverse(visitor);
        }
      }
      types::Type::Tuple(tuple_type) => {
        for element_type in &tuple_type.0 {
          element_type.traverse(visitor);
        }
      }
      types::Type::Object(object_type) => {
        for field_type in &object_type.fields {
          field_type.1.traverse(visitor);
        }
      }
      types::Type::Pointer(pointee_type) => {
        pointee_type.traverse(visitor);
      }
      types::Type::Reference(pointee_type) => {
        pointee_type.traverse(visitor);
      }
      _ => {}
    }
  }
}

impl Visitable for ast::Generics {
  fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
    visitor.default_value()
  }

  fn traverse_children<T>(&self, visitor: &mut dyn Visitor<T>) {
    for generic_type in &self.parameters {
      // OPTIMIZE: Cloning.
      types::Type::Generic(generic_type.to_owned()).traverse(visitor);
    }
  }
}
