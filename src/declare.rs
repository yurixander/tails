use crate::{ast, auxiliary, diagnostic, instantiation, lowering, symbol_table, types, visit};

pub(crate) type DeclarationMap =
  std::collections::HashMap<symbol_table::Qualifier, symbol_table::Scope>;

#[derive(Default)]
pub struct DeclarationContext {
  pub(crate) module_scope: symbol_table::Scope,
  pub(crate) symbol_table: symbol_table::SymbolTable,
  pub(crate) diagnostics: Vec<diagnostic::Diagnostic>,
  instance_base_name_buffer: Option<String>,
  current_function_id: Option<symbol_table::RegistryId>,
}

// CONSIDER: A global, default scope. This would be great to declare the prelude as well.
impl<'a> DeclarationContext {
  fn try_auto_declare_global(&mut self, item: &ast::Item) {
    // Certain items require special handling of declaration procedure.
    if let ast::Item::Function(function) = item {
      self.try_declare_function(&function);

      return;
    }

    if let Some(registry_id) = item.find_registry_id() {
      let name_opt = match item {
        ast::Item::ForeignFunction(foreign_function) => Some(&foreign_function.name),
        ast::Item::ForeignVar(foreign_var) => Some(&foreign_var.name),
        ast::Item::Constant(constant) => Some(&constant.name),
        _ => None,
      };

      if let Some(name) = name_opt {
        self.try_declare(
          symbol_table::Symbol::new(name.to_owned(), symbol_table::SymbolKind::Declaration),
          *registry_id,
        );
      }
    }

    // CONSIDER: Having an `is_global` utility method for nodes, and then `find_symbol` and combining these two for any future global nodes, that way we don't have to "manually" keep track of them here.
  }

  fn try_declare_function(&mut self, function: &ast::Function) {
    let path = if let Some(base_path) = &self.instance_base_name_buffer {
      symbol_table::SymbolPath {
        base_name: base_path.to_owned(),
        sub_name: Some(function.name.to_owned()),
        kind: symbol_table::SymbolKind::Declaration,
      }
    } else {
      symbol_table::SymbolPath {
        // REVISE: Change the system to be dynamically scoped under anything, not specific things as it is now. Then, this function path should be declared under the current parent scope name, otherwise globally in the current module.
        base_name: function.name.to_owned(),
        sub_name: None,
        kind: symbol_table::SymbolKind::Declaration,
      }
    };

    self.try_declare(symbol_table::Symbol { path }, function.registry_id);
  }

  /// Register a symbol to a binding id in the current scope.
  ///
  /// Returns `false`, and creates an error diagnostic in the local diagnostic builder, if
  /// the symbol was already defined in the current scope, or `true` if it was successfully
  /// registered.
  fn try_declare(&mut self, symbol: symbol_table::Symbol, id: symbol_table::RegistryId) -> bool {
    if self.is_declared(&symbol.path) {
      // If the symbol is already declared, issue a re-definition diagnostic.
      self.diagnostics.push(diagnostic::Diagnostic::Redefinition(
        symbol.path.to_string(),
      ));

      return false;
    }

    // FIXME: Must have an assertion that ...

    self.bind(symbol, id);

    true
  }

  /// Bind a global symbol to a given node by its id, on the current scope.
  ///
  /// If the symbol is already bound, its entry value will be overwritten.
  fn bind(&mut self, symbol: symbol_table::Symbol, registry_id: symbol_table::RegistryId) {
    self
      .module_scope
      .insert(symbol.path.to_owned(), (registry_id, symbol));
  }

  /// Determine if the current scope contains the given symbol.
  fn is_declared(&mut self, symbol_path: &symbol_table::SymbolPath) -> bool {
    self.module_scope.contains_key(symbol_path)
  }
}

impl<'a> visit::Visitor for DeclarationContext {
  fn default_value(&mut self) {
    //
  }

  fn enter_item(&mut self, item: &ast::Item) {
    // Register certain items on the registry for later retrieval.
    if let Some(registry_id) = item.find_registry_id() {
      self.symbol_table.registry.insert(
        *registry_id,
        symbol_table::RegistryItem::try_from(item.to_owned())
          .expect(auxiliary::BUG_REGISTRY_ITEM_MUST_BE_ITEM),
      );
    }

    self.try_auto_declare_global(item);

    if let ast::Item::Function(function) = item {
      self.current_function_id = Some(function.registry_id);
    }
  }

  fn exit_item(&mut self, item: &ast::Item) {
    if let ast::Item::Function(_) = item {
      self.current_function_id = None;
    }
  }

  fn enter_expr(&mut self, expr: &ast::Expr) {
    // REVISE: Find a way to avoid repeating this logic for items and expressions. Perhaps use traits to share common logic?
    // Register certain expressions on the registry for later retrieval.
    if let Some(registry_id) = expr.find_registry_id() {
      self.symbol_table.registry.insert(
        *registry_id,
        symbol_table::RegistryItem::try_from(expr.to_owned())
          .expect(auxiliary::BUG_REGISTRY_ITEM_MUST_BE_ITEM),
      );
    }

    if let ast::Expr::CallSite(call_site) = expr {
      // Only register call sites that have generic hints, and are thus
      // considered polymorphic.
      if !call_site.generic_hints.is_empty() {
        self.symbol_table.artifacts.insert(
          call_site.universe_id.to_owned(),
          instantiation::Artifact::CallSite(std::rc::Rc::clone(call_site)),
        );
      }
    }
  }

  fn visit_call_site(&mut self, call_site: &ast::CallSite) {
    self.symbol_table.call_site_parent_functions.insert(
      call_site.universe_id.to_owned(),
      self
        .current_function_id
        .expect("call sites should always be present inside functions"),
    );
  }

  fn visit_function(&mut self, function: &ast::Function) {
    // REVISE: This must be checked only within the initial package. Currently, the main function can be defined elsewhere on its dependencies (even if they're libraries).
    if function.name == lowering::ENTRY_POINT_FUNCTION_NAME {
      if self.symbol_table.entry_function_id.is_some() {
        self
          .diagnostics
          .push(diagnostic::Diagnostic::MultipleEntryPoints);
      } else {
        // FIXME: Store this during the declaration context.
        self.symbol_table.entry_function_id = Some(function.registry_id);
      }
    }
  }

  fn visit_signature(&mut self, signature: &ast::Signature) {
    // NOTE: Parameters should be registered here, so that
    // the `Rc` can be cloned.
    for parameter in &signature.parameters {
      self.symbol_table.registry.insert(
        parameter.registry_id,
        symbol_table::RegistryItem::Parameter(std::rc::Rc::clone(&parameter)),
      );
    }
  }

  fn visit_type_def(&mut self, type_def: &ast::TypeDef) {
    self.try_declare(
      symbol_table::Symbol::new(type_def.name.to_owned(), symbol_table::SymbolKind::Type),
      type_def.registry_id,
    );
  }

  fn visit_union(&mut self, union: &ast::Union) {
    self.try_declare(
      symbol_table::Symbol::new(union.name.to_owned(), symbol_table::SymbolKind::Type),
      union.registry_id,
    );

    // NOTE: Union variants are declared here instead of their corresponding
    // visitor function because here we have direct access to the union, whereas
    // in the variant visitor function it would be necessary to retrieve the sum
    // type from the symbol table, which is not possible during name resolution.
    for variant in &union.variants {
      self.try_declare(
        symbol_table::Symbol {
          path: symbol_table::SymbolPath {
            base_name: union.name.to_owned(),
            sub_name: Some(variant.1.name.to_owned()),
            kind: symbol_table::SymbolKind::Declaration,
          },
        },
        variant.1.registry_id,
      );
    }
  }

  fn visit_type(&mut self, ty: &crate::types::Type) {
    if let types::Type::Stub(stub_type) = ty {
      // BUG: What about stub types that are 'synthesized' after instantiation? ie. types that are instantiated may be artifacts themselves, but they're never registered as such, nor instantiated? If that doesn't apply, add a note explaining why. Same for the call site artifact case above.
      // Only register stub types that have generic hints, and are thus
      // considered polymorphic.
      // FIXME: What if these stubs types when stripped yield polymorphic stub types? The previous logic included stripping logic for type stubs.
      if !stub_type.generic_hints.is_empty() {
        self.symbol_table.artifacts.insert(
          stub_type.universe_id.to_owned(),
          instantiation::Artifact::StubType(stub_type.to_owned()),
        );
      }
    }
    // Register any generic types that are part of functions in the symbol
    // table for later use. This will allow for the instantiation phase to have
    // a direct, linear registry of all generic types within functions, thus
    // avoiding having to traverse the AST to find them.
    else if let types::Type::Generic(generic_type) = ty {
      self.symbol_table.registry.insert(
        generic_type.registry_id,
        symbol_table::RegistryItem::GenericType(generic_type.to_owned()),
      );

      // NOTE: Function id buffer might not always be set when visiting
      // generic types. For example, the generic type could be associated
      // to a type definition instead of being present inside a function.
      if let Some(current_function_id) = self.current_function_id {
        self
          .symbol_table
          .nested_generics
          .entry(current_function_id)
          .and_modify(|generics| generics.push(generic_type.to_owned()))
          .or_insert_with(|| vec![generic_type.to_owned()]);
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn scope_contains() {
    let mut declare_ctx = DeclarationContext::default();
    let symbol = symbol_table::tests::mock_symbol();

    assert!(!declare_ctx.is_declared(&symbol.path));
    declare_ctx.bind(symbol.clone(), symbol_table::RegistryId(0));
    assert!(declare_ctx.is_declared(&symbol.path));
  }

  #[test]
  fn declare_global() {
    let id = symbol_table::RegistryId(0);
    let symbol = symbol_table::tests::mock_symbol();
    let mut declare_ctx = DeclarationContext::default();

    assert!(declare_ctx.try_declare(symbol.clone(), id));
    assert!(declare_ctx.diagnostics.is_empty());
    assert!(!declare_ctx.try_declare(symbol.clone(), id));
    assert_eq!(1, declare_ctx.diagnostics.len());
    assert!(declare_ctx.is_declared(&symbol.path));
  }

  // TODO: Add more tests.
}
