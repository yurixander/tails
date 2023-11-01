//! Contains metadata and location information for applicable nodes of the AST.
//!
//! Since the AST is immutable, a symbol table is used to associate node ids with their respective
//! declarations. This symbol table can then be used by subsequent passes to resolve references to their
//! respective declarations.
//!
//! During the name resolution phases, declarations and any nodes that can be referenced are also cached
//! into a hash map. This allows for fast lookup of declarations and nodes by their respective ids, instead of
//! having to traverse the AST every time a declaration or node pointed to by a reference is needed.
//!
//! The name resolution pass is responsible for resolving all names in the AST to their respective
//! declarations. This pass is also responsible for checking for name conflicts and other errors
//! related to symbols and declarations.

use crate::{ast, instantiation, types};

/// A unique, exclusive identifier for declaration nodes.
///
/// Used to exclusively uniquely identify and retrieve nodes stored on the
/// symbol table.
///
/// Any node that has a declaration id will be automatically stored into the
/// symbol table during the declare step.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct RegistryId(pub usize);

/// A unique identifier for a type.
///
/// The type isn't necessarily bound per-node, or to encompass the entire
/// node's type.
///
/// Furthermore, multiple type ids may point to the same type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub usize);

/// A unique id representing the key of an entry on a type substitution table,
/// used for unification and instantiation.
///
/// Substituted types are meta types that represent other types, and which decay
/// into concrete types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubstitutionId(pub usize);

/// An intermediary unique id that can be used to serve as a middleman
/// resolution step for node ids on the symbol table.
///
/// Used to associate paths/references with a corresponding node id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct LinkId(pub usize);

/// An instantiation artifact is an item that may reference a polymorphic
/// item, and that may provide hints for its instantiation. For example,
/// a call site to a polymorphic function is considered an instantiation
/// artifact.
///
/// Artifact ids uniquely identify such artifacts, and are primarily used
/// for the retrieval of the artifact's generic substitution environment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UniverseId(pub usize, pub String);

/// A type environment that contains the instantiated types of various nodes.
///
/// All type aliases have been resolved before being stored here, therefore
/// stored types are guaranteed to be resolved, and do not further any alias resolution.
pub type TypeEnvironment = std::collections::HashMap<TypeId, types::Type>;

/// A mapping of type variables or generics to other type variables or monomorphic types.
/// Also known as a universe of types.
///
/// This is used during type unification as the substitution environment used
/// to resolve constraints.
pub type SubstitutionEnv = std::collections::BTreeMap<SubstitutionId, types::Type>;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum SymbolKind {
  /// A node declaration, such as a function, parameter or a binding.
  Declaration,
  /// A type definition or aliasing.
  Type,
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum SymbolFlag {
  Shadow,
  Capture,
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct Symbol {
  pub path: SymbolPath,
}

impl Symbol {
  pub fn new(base_name: String, symbol_kind: SymbolKind) -> Self {
    Self {
      path: SymbolPath {
        base_name,
        sub_name: None,
        kind: symbol_kind,
      },
    }
  }
}

// CONSIDER: Revamping the path system. Instead of allowing a specific set number of path names, make it a vector, with one required name (the last name, or the entity name). Everything else in the vector is simply pathing to reach that entity. This will also prevent other logic from depending on pathing since it would lead to possible logic errors (ex. module name, when expecting trait name). The only always-valid name would be the entity name. This can also open the door for more flexible declarations such as constants or type aliases inside instances.
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct SymbolPath {
  pub base_name: String,
  pub sub_name: Option<String>,
  pub kind: SymbolKind,
}

impl std::fmt::Display for SymbolPath {
  fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if let Some(sub_name) = &self.sub_name {
      formatter.write_fmt(format_args!("{}::{}", self.base_name, sub_name))
    } else {
      formatter.write_str(&self.base_name)
    }
  }
}

pub type Scope = std::collections::HashMap<SymbolPath, ScopeEntry>;

pub type ScopeEntry = (RegistryId, Symbol);

#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct Qualifier {
  pub package_name: String,
  pub module_name: String,
}

enum SymbolMetadataFlag {
  Inline,
}

struct SymbolLocation {
  file_path: std::path::PathBuf,
  position: usize,
  length: usize,
}

struct SymbolMetadata {
  pub location: SymbolLocation,
  pub flags: std::collections::HashSet<SymbolMetadataFlag>,
}

// TODO: Make use of.
struct SymbolTableEntry {
  pub item: std::rc::Rc<ast::Item>,
  pub visibility: Visibility,
  pub metadata: SymbolMetadata,
}

pub enum Visibility {
  Public,
  Private,
  Internal,
}

#[derive(Debug, Clone)]
pub enum RegistryItem {
  ForeignFunction(std::rc::Rc<ast::ForeignFunction>),
  ForeignStatic(std::rc::Rc<ast::ForeignStatic>),
  Function(std::rc::Rc<ast::Function>),
  Parameter(std::rc::Rc<ast::Parameter>),
  Union(std::rc::Rc<ast::Union>),
  UnionVariant(std::rc::Rc<ast::UnionVariant>),
  GenericType(types::GenericType),
  Binding(std::rc::Rc<ast::Binding>),
  TypeDef(std::rc::Rc<ast::TypeDef>),
  Constant(std::rc::Rc<ast::Constant>),
  CallSite(std::rc::Rc<ast::CallSite>),
  Closure(std::rc::Rc<ast::Closure>),
  ClosureCapture(std::rc::Rc<ast::ClosureCapture>),
}

impl RegistryItem {
  pub(crate) fn into_item(&self) -> Option<ast::Item> {
    Some(match self {
      RegistryItem::Binding(binding) => ast::Item::Binding(binding.to_owned()),
      RegistryItem::Function(function) => ast::Item::Function(function.to_owned()),
      RegistryItem::Union(union) => ast::Item::Union(union.to_owned()),
      RegistryItem::TypeDef(type_def) => ast::Item::TypeDef(type_def.to_owned()),
      RegistryItem::Constant(constant) => ast::Item::Constant(constant.to_owned()),
      RegistryItem::Parameter(parameter) => ast::Item::Parameter(parameter.to_owned()),
      RegistryItem::ClosureCapture(closure_capture) => {
        ast::Item::ClosureCapture(closure_capture.to_owned())
      }
      RegistryItem::ForeignFunction(foreign_function) => {
        ast::Item::ForeignFunction(foreign_function.to_owned())
      }
      RegistryItem::ForeignStatic(foreign_var) => ast::Item::ForeignVar(foreign_var.to_owned()),
      RegistryItem::UnionVariant(union_variant) => {
        ast::Item::UnionVariant(union_variant.to_owned())
      }
      _ => return None,
    })
  }

  pub(crate) fn into_expr(&self) -> Option<ast::Expr> {
    match self {
      RegistryItem::Closure(closure) => Some(ast::Expr::Closure(closure.to_owned())),
      RegistryItem::CallSite(call_site) => Some(ast::Expr::CallSite(call_site.to_owned())),
      _ => None,
    }
  }
}

// TODO: Store locations, dimensions (for complex types), and metadata for nodes in the symbol table.
#[derive(Default)]
pub struct SymbolTable {
  // REVIEW: Should this be here?
  pub entry_function_id: Option<RegistryId>,
  // FIXME: Having types as keys won't work; unique ids aren't ignored when comparing types.
  pub(crate) instances:
    std::collections::HashMap<types::Type, std::collections::HashSet<RegistryId>>,
  /// A mapping from a referential id to the id of its target node.
  ///
  /// The reason why this is necessary is, and there is no direct mapping
  /// from id to the target node is because then the target node would need to
  /// be repeated per referential id.
  pub(crate) links: std::collections::HashMap<LinkId, RegistryId>,
  // CONSIDER: Some things that should be saved (ex. `SumTypeVariant`) are not to be nodes themselves. Consider having an exclusive enum for possible cached declarations, then implementing a `From` or `Into` trait to convert between node and such trait. With this approach we don't have to implement all kinds of stuff like dispatch entry, and so on. Needs research however, might not be possible or not what it seems.
  /// A hash map containing unique declaration ids to their corresponding AST nodes.
  ///
  /// This is used for direct access to declarations or other reference-able nodes,
  /// which would otherwise require an AST traversal to find. By storing it using a
  /// hash map, direct access is guaranteed on subsequent phases.
  pub(crate) registry: std::collections::HashMap<RegistryId, RegistryItem>,
  pub(crate) call_site_parent_functions: std::collections::HashMap<UniverseId, RegistryId>,
  pub(crate) artifacts: std::collections::HashMap<UniverseId, instantiation::Artifact>,
  /// A collection of owned/nested generic types by items, with the intent to provide
  /// a direct and efficient method to acquire all the raw generic types present within
  /// an item, without having to traverse the item to find them.
  ///
  /// Note that this does not include computed generics, such as anonymous generics.
  pub(crate) nested_generics: std::collections::HashMap<RegistryId, Vec<types::GenericType>>,
}

impl SymbolTable {
  pub(crate) fn follow_link(&self, link_id: &LinkId) -> Option<&RegistryItem> {
    self
      .links
      .get(link_id)
      .and_then(|registry_id| self.registry.get(registry_id))
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;

  pub fn mock_qualifier() -> Qualifier {
    Qualifier {
      package_name: String::from("test_package"),
      module_name: String::from("test_module"),
    }
  }

  pub fn mock_symbol() -> Symbol {
    Symbol {
      path: SymbolPath {
        base_name: String::from("test"),
        sub_name: None,
        kind: SymbolKind::Declaration,
      },
    }
  }

  // TODO: Add more tests for this module.
}
