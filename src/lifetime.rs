//! An analysis pass that determines the lifetime of variables, and
//! enforces aliasing rules.
//!
//! If any discrepancies are found regarding lifetimes of variables,
//! corresponding diagnostics are emitted.

use crate::{ast, auxiliary, diagnostic, resolution, symbol_table, types, visit};

enum LifetimeState {
  Live,
  Moved,
}

struct BindingAttributes {
  is_copyable: bool,
}

pub struct LifetimeAnalysisContext<'a> {
  pub(crate) diagnostics: Vec<diagnostic::Diagnostic>,
  symbol_table: &'a symbol_table::SymbolTable,
  resolution_helper: &'a resolution::ResolutionHelper<'a>,
  lifetime_states: std::collections::HashMap<symbol_table::RegistryId, LifetimeState>,
  binding_attributes: std::collections::HashMap<symbol_table::RegistryId, BindingAttributes>,
  // FIXME: This is not being set anywhere. It should be set when entering a new universe.
  universe_stack: resolution::UniverseStack,
  in_call_site: bool,
}

// TODO: After malloc'd pointers reach their end of life, they must be freed. This can be done by building an LLVM `free` instruction, however this means that we'd need access to the LLVM API/module/context during this pass, or at least a way to communicate when and what to free (ex. a symbol table of what things should be freed, and where, that is then passed onto the LLVM pass). Or, to abstract more instead blindly freeing pointers, we can insert instead a call to the `drop` function from a built-in `Drop` trait, so similar to Rust and also similar to deconstructors in C++.
impl<'a> LifetimeAnalysisContext<'a> {
  const BUG_MISSING_ATTRIBUTE: &'static str =
    "all bindings should have corresponding attribute metadata registered";

  pub(crate) fn new(
    symbol_table: &'a symbol_table::SymbolTable,
    resolution_helper: &'a resolution::ResolutionHelper<'a>,
  ) -> Self {
    Self {
      diagnostics: Vec::new(),
      symbol_table,
      resolution_helper,
      lifetime_states: std::collections::HashMap::new(),
      binding_attributes: std::collections::HashMap::new(),
      universe_stack: resolution::UniverseStack::new(),
      in_call_site: false,
    }
  }
}

impl<'a> visit::ArtifactVisitor for LifetimeAnalysisContext<'a> {
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

impl<'a> visit::Visitor for LifetimeAnalysisContext<'a> {
  fn default_value(&mut self) {
    //
  }

  fn enter_expr(&mut self, _expr: &ast::Expr) {
    match _expr {
      ast::Expr::CallSite(_) => {
        self.in_call_site = true;
      }
      _ => {}
    }
  }

  fn exit_expr(&mut self, expr: &ast::Expr) {
    match expr {
      ast::Expr::CallSite(_) => {
        self.in_call_site = false;
      }
      ast::Expr::Block(block) => {
        // TODO: Insert dropping logic for all live bindings here. Rust's approach is to define the `drop` function in a way that it takes ownership of the value being dropped. In other words, there's no actual `Dropped` state, it just performs a move and thus the value is no longer considered live. This way, additional need for magic is avoided. In other words, a call to `free()` isn't explicitly inserted; instead, simply a call to the `Drop` trait for the value is inserted instead (if the value implements the drop trait, of course).

        // FIXME: This won't work, because even after leaving a given scope, bindings might have been defined on a super scope encompassing the current scope. If the states are simply cleared, information would be lost prematurely.
        // When the scope ends, clear all lifetime states of the current
        // block.
        // self.lifetime_states.clear();
      }
      _ => {}
    }
  }

  // TODO: Lifetime on return (yield).

  fn visit_block(&mut self, block: &ast::Block) {
    let yield_type = self
      .resolution_helper
      .resolve_by_id(&block.type_id, self.universe_stack.clone())
      .expect(auxiliary::BUG_MISSING_TYPE);

    // SAFETY: What if the reference comes from the arguments? We might need to attach lifetimes to declarations.
    if matches!(yield_type.as_ref(), types::Type::Reference(_)) {
      self
        .diagnostics
        .push(diagnostic::Diagnostic::CannotYieldTemporaryReference);
    }
  }

  fn visit_binding(&mut self, binding: &ast::Binding) {
    // Mark the binding as live when it first appears.
    self
      .lifetime_states
      .insert(binding.registry_id, LifetimeState::Live);

    // REVISE: (generics) There's a big problem regarding generics. The current system produces the monomorphisms during lowering, however, certain analysis passes occur before lowering. In other words, the analysis passes cannot analyze the monomorphisms! The system must be adjusted so that the monomorphisms are expanded during type unification phase.

    let ty = self
      .resolution_helper
      .resolve_by_id(&binding.type_id, self.universe_stack.clone())
      .expect(auxiliary::BUG_MISSING_TYPE);

    // TODO: Actual implementation of determining which bindings are copyable is missing. This would be done when the traits system is complete. For now, all bindings whose types aren't primitive types are non-copyable.
    let is_copyable = matches!(ty.as_ref(), types::Type::Primitive(_));

    self
      .binding_attributes
      .insert(binding.registry_id, BindingAttributes { is_copyable });
  }

  fn visit_reference(&mut self, reference: &ast::Reference) {
    let target = self
      .symbol_table
      .follow_link(&reference.path.link_id)
      .expect(auxiliary::BUG_NAME_RESOLUTION);

    if let symbol_table::RegistryItem::Binding(binding) = target {
      let lifetime_state = self
        .lifetime_states
        .get(&binding.registry_id)
        .expect("all accessible bindings should have a corresponding lifetime state registered");

      // FIXME: Awaiting more in-depth analysis to prevent from reporting false positives.
      // if let LifetimeState::Moved = lifetime_state {
      //   self
      //     .diagnostics
      //     .push(diagnostic::Diagnostic::BindingUsedAfterMove(
      //       binding.name.to_owned(),
      //     ));
      // }

      let attributes = self
        .binding_attributes
        .get(&binding.registry_id)
        .expect(Self::BUG_MISSING_ATTRIBUTE);

      // If the binding is not copyable, and reference is to the binding occurs
      // within a call site, and the binding is still live, mark it as moved.
      if self.in_call_site
        && !attributes.is_copyable
        && matches!(lifetime_state, LifetimeState::Live)
      {
        self
          .lifetime_states
          .insert(binding.registry_id, LifetimeState::Moved);
      }
    }
  }
}
