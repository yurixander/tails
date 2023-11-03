//! Contains item structures and definitions that represent the abstract
//! syntax tree (AST) of a program.

use crate::{parser, resolution, symbol_table, types};

pub type Package = std::collections::BTreeMap<symbol_table::Qualifier, Module>;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<usize>;

/// A compilation unit representing a single, entire source file.
#[derive(Debug)]
pub struct Module {
  pub qualifier: symbol_table::Qualifier,
  pub global_items: Vec<Item>,
}

#[derive(Debug)]
pub struct Resume {
  pub condition: Expr,
}

#[derive(Debug)]
pub struct Effect {
  pub name: String,
  pub signature: Signature,
}

#[derive(Debug)]
pub struct EffectHandler {
  pub name: String,
  pub closure: Closure,
}

#[derive(Debug)]
pub struct Try {
  pub expr: Expr,
  pub handlers: Vec<EffectHandler>,
  pub default: Option<EffectHandler>,
}

#[derive(Debug)]
pub struct Raise {
  pub effect_name: String,
  pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum UnionVariantKind {
  String(String),
  Type(types::Type),
  Singleton {
    name: String,
    relative_index: u64,
    explicit_value: Option<i64>,
  },
}

#[derive(Debug, Clone)]
pub struct UnionVariant {
  pub registry_id: symbol_table::RegistryId,
  pub union_id: symbol_table::RegistryId,
  pub name: String,
  pub kind: UnionVariantKind,
}

#[derive(Debug)]
pub struct Union {
  pub registry_id: symbol_table::RegistryId,
  pub name: String,
  pub variants: std::collections::BTreeMap<String, std::rc::Rc<UnionVariant>>,
}

#[derive(Debug)]
pub enum UnionInstanceValue {
  Singleton(String),
  String(String),
  Value(Expr),
}

#[derive(Debug)]
pub struct UnionInstance {
  pub path: Path,
  pub value: UnionInstanceValue,
}

#[derive(Debug)]
pub struct MatchArm {
  pub case: Expr,
  pub body: Expr,
}

#[derive(Debug)]
pub struct Match {
  pub type_id: symbol_table::TypeId,
  pub subject_type_id: symbol_table::TypeId,
  pub subject: Expr,
  pub arms: Vec<MatchArm>,
  pub default_case: Expr,
}

#[derive(Debug)]
pub struct With {
  pub object: Expr,
  pub deltas: Object,
}

#[derive(Debug, Default)]
pub struct Generics {
  pub parameters: Vec<types::GenericType>,
  // TODO: Constraints.
}

/// A parentheses expression.
#[derive(Debug)]
pub struct Group(pub Expr);

#[derive(Debug, Clone)]
pub struct Parameter {
  pub registry_id: symbol_table::RegistryId,
  pub type_id: symbol_table::TypeId,
  pub name: String,
  pub position: parser::LlvmSize,
  pub type_hint: Option<types::Type>,
}

#[derive(Debug, Clone)]
pub enum Expr {
  With(std::rc::Rc<With>),
  Range(std::rc::Rc<Range>),
  CallSite(std::rc::Rc<CallSite>),
  BinaryOp(std::rc::Rc<BinaryOp>),
  UnaryOp(std::rc::Rc<UnaryOp>),
  Object(std::rc::Rc<Object>),
  ObjectAccess(std::rc::Rc<ObjectAccess>),
  Unsafe(std::rc::Rc<Unsafe>),
  Group(std::rc::Rc<Group>),
  Sizeof(std::rc::Rc<Sizeof>),
  Cast(std::rc::Rc<Cast>),
  Match(std::rc::Rc<Match>),
  Tuple(std::rc::Rc<Tuple>),
  TupleIndexing(std::rc::Rc<TupleIndex>),
  Try(std::rc::Rc<Try>),
  Resume(std::rc::Rc<Resume>),
  Discard(std::rc::Rc<Discard>),
  PointerIndexing(std::rc::Rc<PointerIndexing>),
  // REVISE: This should not be an Rc; `Pass` is an empty struct. In fact, it may even be left out entirely (as `Item::Pass`).
  Pass(std::rc::Rc<Pass>),
  Reference(std::rc::Rc<Reference>),
  If(std::rc::Rc<If>),
  Literal(Literal),
  Statement(std::rc::Rc<Statement>),
  Closure(std::rc::Rc<Closure>),
  UnionInstance(std::rc::Rc<UnionInstance>),
  Block(std::rc::Rc<Block>),
}

impl Expr {
  /// Flatten this expression, extracting the inner node of any transient
  /// containers.
  ///
  /// This will not resolve any references, and will not follow any
  /// references. Statements are not considered transient, and will
  /// not be flattened.
  pub(crate) fn flatten(&self) -> &Expr {
    match self {
      Expr::Group(group) => group.0.flatten(),
      Expr::Unsafe(unsafe_) => unsafe_.0.flatten(),
      _ => self,
    }
  }

  // CONSIDER: Getting rid of this. It's only used once (for statements).
  /// Attempt to find the type id of the item.
  ///
  /// This function examines the node after stripping transient
  /// layers and returns the type id associated with the node,
  /// if it exists. For items without an associated type id, return
  /// `None`.
  pub(crate) fn find_type_id(&self) -> Option<&symbol_table::TypeId> {
    match self {
      Expr::Match(match_) => Some(&match_.type_id),
      Expr::BinaryOp(binary_op) => Some(&binary_op.type_id),
      Expr::UnaryOp(unary_op) => Some(&unary_op.type_id),
      Expr::Object(object) => Some(&object.type_id),
      Expr::ObjectAccess(object_access) => Some(&object_access.type_id),
      Expr::Sizeof(sizeof_) => Some(&sizeof_.type_id),
      Expr::Cast(cast) => Some(&cast.type_id),
      Expr::Tuple(tuple) => Some(&tuple.type_id),
      Expr::TupleIndexing(tuple_indexing) => Some(&tuple_indexing.type_id),
      Expr::Literal(literal) => Some(&literal.type_id),
      Expr::Closure(closure) => Some(&closure.type_id),
      Expr::Reference(reference) => Some(&reference.type_id),
      Expr::CallSite(call_site) => Some(&call_site.type_id),
      Expr::PointerIndexing(pointer_indexing) => Some(&pointer_indexing.type_id),
      Expr::Block(block) => Some(&block.type_id),
      Expr::If(if_) => Some(&if_.type_id),
      _ => None,
    }
  }

  pub(crate) fn find_registry_id(&self) -> Option<&symbol_table::RegistryId> {
    match self {
      Expr::CallSite(call_site) => Some(&call_site.registry_id),
      Expr::Closure(closure) => Some(&closure.registry_id),
      _ => None,
    }
  }

  pub(crate) fn find_debug_name(&self) -> Option<String> {
    match self.flatten() {
      Expr::Reference(reference) => Some(&reference.path.base_name),
      _ => None,
    }
    .map(|debug_name| debug_name.to_owned())
  }
}

impl TryFrom<Expr> for symbol_table::RegistryItem {
  type Error = ();

  fn try_from(value: Expr) -> Result<Self, Self::Error> {
    match value {
      Expr::CallSite(call_site) => Ok(symbol_table::RegistryItem::CallSite(call_site.clone())),
      Expr::Closure(closure) => Ok(symbol_table::RegistryItem::Closure(closure.clone())),
      _ => Err(()),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Item {
  ForeignFunction(std::rc::Rc<ForeignFunction>),
  ForeignVar(std::rc::Rc<ForeignStatic>),
  Function(std::rc::Rc<Function>),
  Binding(std::rc::Rc<Binding>),
  Parameter(std::rc::Rc<Parameter>),
  TypeDef(std::rc::Rc<TypeDef>),
  ClosureCapture(std::rc::Rc<ClosureCapture>),
  Import(std::rc::Rc<Import>),
  Union(std::rc::Rc<Union>),
  UnionVariant(std::rc::Rc<UnionVariant>),
  Effect(std::rc::Rc<Effect>),
  ForeignCluster(std::rc::Rc<ForeignCluster>),
  Constant(std::rc::Rc<Constant>),
  PointerAssignment(std::rc::Rc<PointerAssignment>),
}

impl Item {
  pub(crate) fn is_polymorphic(&self) -> bool {
    self
      .find_generics()
      .map(|generics| !generics.parameters.is_empty())
      .unwrap_or(false)
  }

  pub(crate) fn find_generics(&self) -> Option<&Generics> {
    match self {
      Item::Function(function) => Some(&function.generics),
      Item::TypeDef(type_def) => Some(&type_def.generics),
      _ => None,
    }
  }

  /// Attempt to find the declaration id of this node.
  ///
  /// Mainly reference-able nodes have declaration ids, and not all values
  /// have declaration ids. Transient nodes (such as groups) will return their
  /// underlying node's declaration id (if any).
  pub(crate) fn find_registry_id(&self) -> Option<&symbol_table::RegistryId> {
    match self {
      Item::Binding(binding) => Some(&binding.registry_id),
      Item::Function(function) => Some(&function.registry_id),
      Item::TypeDef(type_def) => Some(&type_def.registry_id),
      Item::ClosureCapture(closure_capture) => Some(&closure_capture.registry_id),
      Item::ForeignFunction(foreign_function) => Some(&foreign_function.registry_id),
      Item::ForeignVar(foreign_var) => Some(&foreign_var.registry_id),
      Item::Parameter(parameter) => Some(&parameter.registry_id),
      Item::Union(union) => Some(&union.registry_id),
      Item::UnionVariant(union_variant) => Some(&union_variant.registry_id),
      Item::Constant(constant) => Some(&constant.registry_id),
      // Most non-value nodes have no associated declaration id because
      // there is no need for them. This also implies that they have no
      // type associated in the type environment, and thus are implicitly
      // of type unit.
      _ => None,
    }
  }
}

impl TryFrom<Item> for symbol_table::RegistryItem {
  type Error = ();

  fn try_from(value: Item) -> Result<Self, Self::Error> {
    Ok(match value {
      Item::Function(function) => symbol_table::RegistryItem::Function(function.clone()),
      Item::Binding(binding) => symbol_table::RegistryItem::Binding(binding.clone()),
      Item::Parameter(parameter) => symbol_table::RegistryItem::Parameter(parameter.clone()),
      Item::TypeDef(type_def) => symbol_table::RegistryItem::TypeDef(type_def.clone()),
      Item::Union(union) => symbol_table::RegistryItem::Union(union.clone()),
      Item::Constant(constant) => symbol_table::RegistryItem::Constant(constant.clone()),
      Item::ClosureCapture(closure_capture) => {
        symbol_table::RegistryItem::ClosureCapture(closure_capture.clone())
      }
      Item::UnionVariant(union_variant) => {
        symbol_table::RegistryItem::UnionVariant(union_variant.clone())
      }
      Item::ForeignVar(foreign_var) => {
        symbol_table::RegistryItem::ForeignStatic(foreign_var.clone())
      }
      Item::ForeignFunction(foreign_function) => {
        symbol_table::RegistryItem::ForeignFunction(foreign_function.clone())
      }
      // TODO: Effect.
      _ => return Err(()),
    })
  }
}

#[derive(Debug, Clone)]
pub struct Constant {
  pub registry_id: symbol_table::RegistryId,
  pub name: String,
  pub ty: types::Type,
  pub value: Box<Expr>,
  /// Whether the constant was declared inside of a local scope, such as
  /// a function body or block.
  pub was_declared_locally: bool,
}

#[derive(Debug)]
pub struct Discard(pub Expr);

#[derive(Debug)]
pub struct TupleIndex {
  pub type_id: symbol_table::TypeId,
  /// The index of the accessed tuple element.
  ///
  /// Note that tuple indexing is type-checked at compile-time,
  /// therefore indexing is guaranteed to be valid.
  pub index: u32,
  pub indexed_tuple: Expr,
  pub indexed_tuple_type_id: symbol_table::TypeId,
}

#[derive(Debug)]
pub struct Tuple {
  pub type_id: symbol_table::TypeId,
  pub elements: Vec<Expr>,
}

#[derive(Debug)]
pub struct Range {
  pub start: u64,
  pub end: u64,
}

#[derive(Debug)]
pub struct Import {
  pub package_name: Option<String>,
  pub module_name: String,
}

#[derive(Debug, Clone)]
pub struct ClosureCapture {
  pub name: String,
  pub registry_id: symbol_table::RegistryId,
  pub closure_registry_id: symbol_table::RegistryId,
  pub index: u32,
  pub target_link_id: symbol_table::LinkId,
  pub type_id: symbol_table::TypeId,
}

#[derive(Debug)]
pub struct Closure {
  pub registry_id: symbol_table::RegistryId,
  pub type_id: symbol_table::TypeId,
  pub captures: Vec<ClosureCapture>,
  pub signature: std::rc::Rc<Signature>,
  pub body: Expr,
}

#[derive(Debug)]
pub struct Pass;

#[derive(Debug)]
pub struct ForeignCluster {
  pub foreigns: Vec<Item>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Path {
  pub link_id: symbol_table::LinkId,
  /// The basic resolution details for this path.
  ///
  /// These include what module and, optionally, package this entity pertains to.
  /// If omitted, the path is assumed to target an entity in the local module.
  pub qualifier: Option<symbol_table::Qualifier>,
  /// The base, target entity.
  ///
  /// This would usually be functions, global static foreign variables, foreign functions,
  /// etc.
  pub base_name: String,
  /// An optional static member entity, part of a struct.
  pub sub_name: Option<String>,
  /// Specifies what kind of entity this path is looking for.
  pub symbol_kind: symbol_table::SymbolKind,
}

#[derive(Debug)]
pub struct Object {
  pub type_id: symbol_table::TypeId,
  pub fields: std::collections::HashMap<String, Expr>,
}

#[derive(Debug)]
pub struct Unsafe(pub Expr);

#[derive(Debug)]
pub struct Reference {
  pub type_id: symbol_table::TypeId,
  pub path: Path,
}

impl Reference {
  /// Attempt to strip a single reference layer.
  ///
  /// If the operation fails for reasons other than the item
  /// not being a reference, a corresponding `Err` variant will be
  /// returned with the cause, otherwise the same item will be returned
  /// under the `Ok` variant.
  pub(crate) fn strip_once(
    &self,
    symbol_table: &symbol_table::SymbolTable,
  ) -> Result<Item, &'static str> {
    // REVISE: Avoid deep nesting.
    if let Some(target) = symbol_table.follow_link(&self.path.link_id) {
      if let Some(target_item) = target.into_item() {
        return Ok(target_item);
      } else {
        return Err("target cannot be converted into an item");
      }
    } else {
      return Err("link is not registered in the symbol table for this reference");
    }
  }
}

#[derive(Debug)]
pub struct Sizeof {
  pub ty: types::Type,
  pub type_id: symbol_table::TypeId,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
  Bool(bool),
  Char(char),
  String(String),
  Nullptr(Option<types::Type>),
  Number {
    value: f64,
    is_real: bool,
    bit_width: types::BitWidth,
    /// A type hint qualifying the type of this number.
    ///
    /// ## Note
    ///
    /// Only for use during type inference; all later stages should rely
    /// on a type environment to resolve the type of this AST node instead.
    type_hint: Option<types::Type>,
  },
}

#[derive(Debug, Clone)]
pub struct Literal {
  pub type_id: symbol_table::TypeId,
  pub kind: LiteralKind,
}

#[derive(Debug)]
pub enum SignatureKind {
  Closure,
  Function,
  ForeignFunction,
  InstanceMethod(Parameter),
  Effect,
}

#[derive(Debug)]
pub struct Signature {
  pub parameters: Vec<std::rc::Rc<Parameter>>,
  /// A type hint qualifying the return type of this signature.
  ///
  /// ## Note
  ///
  /// Only for use during type inference; all later stages should rely
  /// on a type environment to resolve the type of this AST node instead.
  pub return_type_hint: Option<types::Type>,
  pub is_variadic: bool,
  pub kind: SignatureKind,
  pub return_type_id: symbol_table::TypeId,
  pub effects_used: Vec<String>,
}

impl Signature {
  /// Convert this signature into a signature type.
  ///
  /// This will return `None` if a parameter does not have an
  /// associated type on the given type environment.
  pub(crate) fn as_signature_type<'a>(
    &self,
    return_type: types::Type,
    resolution_helper: &resolution::ResolutionHelper<'a>,
    universe_stack: resolution::UniverseStack,
  ) -> Result<types::SignatureType, resolution::TypeResolutionByIdError> {
    let mut parameter_types = Vec::with_capacity(self.parameters.len());

    for parameter in &self.parameters {
      let parameter_type = resolution_helper
        .resolve_by_id(&parameter.type_id, universe_stack.clone())?
        .into_owned();

      parameter_types.push(parameter_type);
    }

    let arity_mode = if self.is_variadic {
      types::ArityMode::Variadic {
        minimum_required_parameters: self.parameters.len(),
      }
    } else {
      types::ArityMode::Fixed
    };

    Ok(types::SignatureType {
      return_type: Box::new(return_type),
      parameter_types,
      arity_mode,
    })
  }
}

#[derive(Debug)]
pub struct ForeignFunction {
  pub registry_id: symbol_table::RegistryId,
  pub type_id: symbol_table::TypeId,
  pub name: String,
  pub signature: std::rc::Rc<Signature>,
}

#[derive(Debug)]
pub struct ForeignStatic {
  pub registry_id: symbol_table::RegistryId,
  pub name: String,
  pub ty: types::Type,
}

#[derive(Debug)]
pub struct Function {
  pub registry_id: symbol_table::RegistryId,
  pub type_id: symbol_table::TypeId,
  pub name: String,
  pub signature: std::rc::Rc<Signature>,
  pub body: std::rc::Rc<Block>,
  pub generics: Generics,
}

impl Function {
  pub fn is_polymorphic(&self) -> bool {
    !self.generics.parameters.is_empty()
  }
}

#[derive(Debug)]
pub struct Block {
  pub type_id: symbol_table::TypeId,
  pub statements: Vec<std::rc::Rc<Statement>>,
  pub yield_value: Expr,
}

#[derive(Debug)]
pub struct Binding {
  pub registry_id: symbol_table::RegistryId,
  pub type_id: symbol_table::TypeId,
  pub name: String,
  pub value: Expr,
  /// A type hint qualifying the type of this binding's value.
  ///
  /// ## Note
  ///
  /// Only for use during type inference; all later stages should rely
  /// on a type environment to resolve the type of this AST node instead.
  pub type_hint: Option<types::Type>,
}

#[derive(Debug)]
pub struct If {
  pub type_id: symbol_table::TypeId,
  pub condition: Expr,
  pub then_branch: Expr,
  pub elif_branches: Vec<(Expr, Expr)>,
  pub else_branch: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Statement {
  Binding(std::rc::Rc<Binding>),
  Constant(std::rc::Rc<Constant>),
  PointerAssignment(std::rc::Rc<PointerAssignment>),
  InlineExpr(Expr),
}

#[derive(Debug)]
pub struct CallSiteArg {
  pub type_id: symbol_table::TypeId,
  pub value: Expr,
}

#[derive(Debug, Clone)]
pub enum Callable {
  Closure(std::rc::Rc<Closure>),
  Function(std::rc::Rc<Function>),
  ForeignFunction(std::rc::Rc<ForeignFunction>),
}

impl Callable {
  pub(crate) fn find_display_name(&self) -> Option<String> {
    match self {
      Callable::ForeignFunction(foreign_function) => Some(foreign_function.name.to_owned()),
      Callable::Function(function) => Some(function.name.to_owned()),
      Callable::Closure(closure) => Some(format!("closure#{}", closure.registry_id.0)),
    }
  }

  pub(crate) fn get_registry_id(&self) -> symbol_table::RegistryId {
    match self {
      Callable::ForeignFunction(foreign_function) => foreign_function.registry_id,
      Callable::Function(function) => function.registry_id,
      Callable::Closure(closure) => closure.registry_id,
    }
  }
}

#[derive(Debug)]
pub struct CallSite {
  pub registry_id: symbol_table::RegistryId,
  pub universe_id: symbol_table::UniverseId,
  pub type_id: symbol_table::TypeId,
  pub callee_expr: Expr,
  pub callee_type_id: symbol_table::TypeId,
  pub arguments: Vec<CallSiteArg>,
  pub generic_hints: Vec<types::Type>,
}

impl CallSite {
  pub fn strip_callee(
    &self,
    symbol_table: &symbol_table::SymbolTable,
  ) -> Result<Callable, &'static str> {
    const NOT_CALLABLE_ERR: &str = "callee is not actually a callable expression";
    const MAX_DEBUG_ITERATIONS: usize = 10_000;

    // OPTIMIZE: Avoid cloning.
    let mut current = self.callee_expr.to_owned();

    let mut debug_iterations = 0;

    loop {
      match current.flatten() {
        Expr::Closure(closure) => return Ok(Callable::Closure(closure.clone())),
        Expr::CallSite(call_site) => return call_site.strip_callee(symbol_table),
        Expr::Reference(reference) => {
          let target_item = reference.strip_once(symbol_table)?;

          match target_item {
            // OPTIMIZE: Avoid cloning.
            Item::Binding(binding) => current = binding.value.clone(),
            Item::Function(function) => return Ok(Callable::Function(function)),
            Item::ForeignFunction(foreign_function) => {
              return Ok(Callable::ForeignFunction(foreign_function))
            }
            _ => return Err(NOT_CALLABLE_ERR),
          }
        }
        _ => return Err(NOT_CALLABLE_ERR),
      }

      debug_iterations += 1;

      debug_assert!(
        debug_iterations < MAX_DEBUG_ITERATIONS,
        "possible infinite loop detected"
      );
    }
  }
}

#[derive(Debug)]
pub struct TypeDef {
  pub registry_id: symbol_table::RegistryId,
  pub name: String,
  pub body: types::Type,
  pub generics: Generics,
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
  Not,
  Negate,
  ReferenceOf,
  Dereference,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOperator {
  And,
  Or,
  Nand,
  Nor,
  Xor,
  Add,
  Subtract,
  Multiply,
  Divide,
  LessThan,
  GreaterThan,
  LessThanOrEqual,
  GreaterThanOrEqual,
  Equality,
  In,
  Inequality,
  Modulo,
}

#[derive(Debug)]
pub struct BinaryOp {
  pub type_id: symbol_table::TypeId,
  pub operand_type_id: symbol_table::TypeId,
  pub operator: BinaryOperator,
  pub left_operand: Expr,
  pub right_operand: Expr,
}

#[derive(Debug)]
pub struct UnaryOp {
  pub type_id: symbol_table::TypeId,
  pub operand_type_id: symbol_table::TypeId,
  pub operator: UnaryOperator,
  pub operand: Expr,
}

#[derive(Debug)]
pub struct Cast {
  pub type_id: symbol_table::TypeId,
  pub operand_type_id: symbol_table::TypeId,
  pub operand: Expr,
  pub cast_type: types::Type,
}

#[derive(Debug)]
pub struct ObjectAccess {
  pub type_id: symbol_table::TypeId,
  pub base_expr_type_id: symbol_table::TypeId,
  pub object: Expr,
  pub field_name: String,
}

#[derive(Debug)]
pub struct PointerIndexing {
  pub type_id: symbol_table::TypeId,
  pub pointer: Expr,
  pub index: Expr,
}

#[derive(Debug)]
pub struct PointerAssignment {
  pub pointer: Expr,
  pub value: Expr,
}
