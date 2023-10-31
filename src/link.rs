use crate::{ast, diagnostic, symbol_table, types, visit};

pub struct LinkContext<'a> {
  pub links: std::collections::HashMap<symbol_table::LinkId, symbol_table::RegistryId>,
  pub(crate) diagnostics: Vec<diagnostic::Diagnostic>,
  module_qualifier: symbol_table::Qualifier,
  scope_stack: Vec<symbol_table::Scope>,
  declarations: &'a std::collections::HashMap<symbol_table::Qualifier, symbol_table::Scope>,
}

impl<'a> LinkContext<'a> {
  /// Determine whether the node has a scope under which definitions can be made.
  ///
  /// This applies to, for example, blocks, functions and closures, as well as
  /// constructs that may accept generics. Transient layers will not be stripped
  /// when matching the item.
  fn does_item_has_scope(item: &ast::Item) -> bool {
    matches!(
      item,
      ast::Item::ForeignFunction(..) | ast::Item::Function(_) | ast::Item::TypeDef(_)
    )
  }

  fn does_expr_has_scope(expr: &ast::Expr) -> bool {
    matches!(expr, ast::Expr::Block(_) | ast::Expr::Closure(_))
  }

  pub fn new(
    declarations: &'a std::collections::HashMap<symbol_table::Qualifier, symbol_table::Scope>,
    module_qualifier: symbol_table::Qualifier,
  ) -> Result<Self, &'static str> {
    if !declarations.contains_key(&module_qualifier) {
      return Err("module qualifier not found in declarations");
    }

    Ok(Self {
      links: std::collections::HashMap::new(),
      module_qualifier,
      scope_stack: Vec::new(),
      declarations,
      diagnostics: Vec::new(),
    })
  }

  /// Lookup a symbol in the global scope of a specific package and module.
  fn global_lookup(
    &self,
    qualifier: &symbol_table::Qualifier,
    symbol_path: &symbol_table::SymbolPath,
  ) -> Option<&symbol_table::ScopeEntry> {
    self
      .declarations
      .get(qualifier)
      .and_then(|global_scope| global_scope.get(&symbol_path))
  }

  /// Lookup a symbol starting from the nearest scope, all the way to the global scope
  /// of the current module.
  fn local_to_global_lookup(
    &self,
    symbol_path: &symbol_table::SymbolPath,
  ) -> Option<&symbol_table::ScopeEntry> {
    // By iterating in reverse, nearest scopes will be checked first.
    for scope in self.scope_stack.iter().rev() {
      if let Some(target_registry_id) = scope.get(symbol_path) {
        return Some(&target_registry_id);
      }
    }

    // If not already found, attempt to find the symbol in the global scope
    // of the current module.
    self.global_lookup(&self.module_qualifier, symbol_path)
  }

  /// Perform a lookup starting from the nearest scope, all the way up the global scope
  /// of the current module. If the symbol is not found, an undefined reference diagnostic
  /// will be added to the diagnostics list, and `None` will be returned.
  fn local_to_global_lookup_or_add_error(
    &mut self,
    symbol_path: &symbol_table::SymbolPath,
  ) -> Option<symbol_table::ScopeEntry> {
    if let Some(entry) = self.local_to_global_lookup(symbol_path) {
      // OPTIMIZE: Avoid cloning.
      return Some(entry.to_owned());
    }

    self
      .diagnostics
      .push(diagnostic::Diagnostic::UndefinedReference(
        symbol_path.to_string(),
      ));

    None
  }

  /// Bind a symbol to a declaration id on the last scope in the scope stack.
  ///
  /// ## Panics
  ///
  /// This operation assumes that the scope stack is not empty. If it is empty,
  /// a panic will occur.
  fn bind_local(&mut self, symbol: symbol_table::Symbol, registry_id: symbol_table::RegistryId) {
    self
      .scope_stack
      .last_mut()
      // CONSIDER: Having this be a `Result` instead of a `panic!`, if it isn't a logic bug. However, if that's done, then all the callers would need to `.expect` or `.unwrap`.
      .expect("there should be at least one scope in the stack when binding a local symbol")
      .insert(symbol.path.clone(), (registry_id, symbol));
  }

  /// Attempt to declare a local symbol table entry. If the given symbol is already
  /// defined in any of the visible scopes, a local re-definition diagnostic will be
  /// issued, and the symbol will not be bound.
  fn try_declare_local(
    &mut self,
    symbol: symbol_table::Symbol,
    id: symbol_table::RegistryId,
    has_shadowing_intent: bool,
  ) {
    // Check for existing definitions that can't be shadowed,
    // and ensure that the symbol being declared is explicitly
    // a shadow.
    if self.local_to_global_lookup(&symbol.path).is_some() && !has_shadowing_intent {
      let symbol_path = symbol.path.to_string();

      self
        .diagnostics
        .push(diagnostic::Diagnostic::Redefinition(symbol_path));

      return;
    }

    // Bind the symbol to the current local scope.
    self.bind_local(symbol, id);
  }
}

impl<'a> visit::Visitor for LinkContext<'a> {
  fn default_value(&mut self) {
    //
  }

  fn enter_item(&mut self, item: &ast::Item) {
    if Self::does_item_has_scope(item) {
      self.scope_stack.push(symbol_table::Scope::new());
    }

    let generics = match item {
      ast::Item::Function(function) => Some(&function.generics),
      ast::Item::TypeDef(type_def) => Some(&type_def.generics),
      _ => None,
    };

    if let Some(generics) = generics {
      for generic_parameter in &generics.parameters {
        self.try_declare_local(
          symbol_table::Symbol {
            path: symbol_table::SymbolPath {
              base_name: generic_parameter.name.to_owned(),
              sub_name: None,
              kind: symbol_table::SymbolKind::Type,
            },
          },
          generic_parameter.registry_id,
          false,
        );
      }
    }
  }

  fn exit_item(&mut self, item: &ast::Item) {
    if Self::does_item_has_scope(item) {
      self.scope_stack.pop();
    }
  }

  fn enter_expr(&mut self, expr: &ast::Expr) {
    if Self::does_expr_has_scope(expr) {
      self.scope_stack.push(symbol_table::Scope::new());
    }
  }

  fn exit_expr(&mut self, expr: &ast::Expr) {
    if Self::does_expr_has_scope(expr) {
      self.scope_stack.pop();
    }
  }

  fn visit_binding(&mut self, binding: &ast::Binding) {
    self.try_declare_local(
      symbol_table::Symbol {
        path: symbol_table::SymbolPath {
          base_name: binding.name.clone(),
          sub_name: None,
          kind: symbol_table::SymbolKind::Declaration,
        },
      },
      binding.registry_id,
      false,
    );
  }

  fn visit_parameter(&mut self, parameter: &ast::Parameter) {
    self.try_declare_local(
      symbol_table::Symbol {
        path: symbol_table::SymbolPath {
          base_name: parameter.name.clone(),
          sub_name: None,
          kind: symbol_table::SymbolKind::Declaration,
        },
      },
      parameter.registry_id,
      false,
    );
  }

  fn visit_path(&mut self, path: &ast::Path) {
    let symbol = symbol_table::Symbol {
      path: symbol_table::SymbolPath {
        base_name: path.base_name.clone(),
        sub_name: path.sub_name.clone(),
        kind: path.symbol_kind.clone(),
      },
    };

    // REVISE: Cleanup.
    if let Some(qualifier) = &path.qualifier {
      // TODO: Abstract and reuse error handling.
      if let Some(target_entry) = self.global_lookup(&qualifier, &symbol.path) {
        self.links.insert(path.link_id, target_entry.0);
      } else {
        self
          .diagnostics
          .push(diagnostic::Diagnostic::QualifiedSymbolNotFound(format!(
            "{}::{}::{}",
            qualifier.package_name, qualifier.module_name, path.base_name
          )));
      }

      return;
    }

    // REVISE: A bit misleading, since `lookup_or_error` returns `Option<>`.
    if let Some(target_entry) = self.local_to_global_lookup_or_add_error(&symbol.path) {
      self.links.insert(path.link_id, target_entry.0);
    }
  }

  fn visit_closure_capture(&mut self, closure_capture: &ast::ClosureCapture) {
    // Link the closure capture to the target item, allowing the
    // retrieval of the target on further stages.
    if let Some(target_entry) =
      self.local_to_global_lookup_or_add_error(&symbol_table::SymbolPath {
        base_name: closure_capture.name.to_owned(),
        sub_name: None,
        kind: symbol_table::SymbolKind::Declaration,
      })
    {
      self
        .links
        .insert(closure_capture.target_link_id, target_entry.0);
    }

    // TODO: Global declarations can be captured currently. Should this be disallowed?

    // The closure will have a scope created for its captures. For every
    // capture, bind a local symbol, with the idea to override or shadow the
    // original captured item's declaration. When the same name is referenced
    // within the closure, it will resolve to the closure capture.
    self.try_declare_local(
      symbol_table::Symbol {
        path: symbol_table::SymbolPath {
          base_name: closure_capture.name.to_owned(),
          sub_name: None,
          kind: symbol_table::SymbolKind::Declaration,
        },
      },
      closure_capture.registry_id,
      // Closure captures are always shadowing other declarations.
      true,
    );

    // TODO: Not necessarily here, but, closure bodies should be isolated from outer scopes, to prevent logic bugs down the road. Perhaps, on enter closure body: save scope stack into buffer, set it as empty. On exit closure body: pop saved scope stack, and restore it. Also considerations need to be made for things like whether local type aliases should be accessible within the closure without the need to capture them. Perhaps during the isolation process, type symbol kinds should remain on the scope stack, and only declaration symbols should be removed. Furthermore, consider making the isolation process a function that is invoked, for semantic meaning. Since it's split into `enter` and `exit`, consider making it two functions: `isolate_closure_scope`, `restore_isolated_closure_scopes`.
  }

  fn visit_type(&mut self, ty: &types::Type) {
    if let types::Type::Stub(stub_type) = ty {
      let symbol = symbol_table::Symbol {
        path: symbol_table::SymbolPath {
          base_name: stub_type.path.base_name.clone(),
          sub_name: stub_type.path.sub_name.clone(),
          kind: symbol_table::SymbolKind::Type,
        },
      };

      if let Some(target_entry) = self.local_to_global_lookup_or_add_error(&symbol.path) {
        self.links.insert(stub_type.path.link_id, target_entry.0);
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn global_lookup() {
    let fake_id = symbol_table::RegistryId(0);
    let mut declarations = std::collections::HashMap::new();
    let mut symbol_mappings = symbol_table::Scope::new();
    let qualifier = symbol_table::tests::mock_qualifier();
    let symbol = symbol_table::tests::mock_symbol();

    symbol_mappings.insert(symbol.path.clone(), (fake_id.clone(), symbol.clone()));
    declarations.insert(qualifier.clone(), symbol_mappings);

    let link_ctx = LinkContext::new(&declarations, qualifier.clone()).unwrap();

    assert_eq!(
      Some(fake_id),
      link_ctx
        .global_lookup(&qualifier, &symbol.path)
        .map(|entry| entry.0)
    );
  }

  #[test]
  fn local_to_global_lookup() {
    let target_id = symbol_table::RegistryId(0);
    let mut declarations = std::collections::HashMap::new();
    let qualifier = symbol_table::tests::mock_qualifier();

    declarations.insert(qualifier.clone(), symbol_table::Scope::new());

    let symbol = symbol_table::tests::mock_symbol();
    let mut link_ctx = LinkContext::new(&declarations, qualifier).unwrap();

    link_ctx.scope_stack.push(std::collections::HashMap::new());
    link_ctx.try_declare_local(symbol.clone(), target_id.clone(), false);

    // FIXME: Because of the way it's built, this panics when it defaults to global lookup.
    // assert!(link_ctx.local_to_global_lookup(&symbol.path).is_none());

    assert_eq!(
      Some(target_id),
      link_ctx
        .local_to_global_lookup(&symbol.path)
        .map(|entry| entry.0)
    );
  }

  // TODO: Need to test everything here.
  // TODO: Add test to ensure that cross-module lookups don't occur, which is a suspected bug.
  // TODO: Simply add an empty global scope to get the test working properly.
}
