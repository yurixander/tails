use crate::symbol_table;

#[macro_export]
macro_rules! assert_extract {
  ($subject:expr, $path:path) => {
    match $subject {
      $path(inner) => inner,
      _ => unreachable!(),
    }
  };
}

pub(crate) const BUG_REGISTRY_ITEM_MUST_BE_ITEM: &str =
  "registry item should be convertible into an item";

pub(crate) const BUG_NAME_RESOLUTION: &str =
  "name resolution should have previously registered all links and nodes in the symbol table";

pub(crate) const BUG_BUFFER_CONTRACT: &str =
  "required buffer should contain a value when this function is invoked";

pub(crate) const BUG_MISSING_TYPE: &str =
  "item or type id should have a corresponding associated type in the type environment";

pub(crate) const BUG_FOREIGN_FN_TYPE_HINTS: &str =
  "foreign functions should always provide full signature type hints";

pub(crate) const MISSING_SYMBOL_TABLE_ENTRY: &str =
  "a required entry is missing on the provided symbol table";

#[derive(Default)]
pub struct IdGenerator {
  counter: usize,
}

impl IdGenerator {
  pub fn new(initial_count: usize) -> Self {
    Self {
      counter: initial_count,
    }
  }

  pub fn get_counter(&self) -> usize {
    self.counter
  }

  /// Create a unique, non-repeating identifier based from a counter.
  ///
  /// This is used to create unique ids for AST node fields and identifiers.
  pub fn next(&mut self) -> usize {
    let id = self.counter;

    self.counter += 1;

    id
  }

  pub fn next_link_id(&mut self) -> symbol_table::LinkId {
    symbol_table::LinkId(self.next())
  }

  pub fn next_registry_id(&mut self) -> symbol_table::RegistryId {
    symbol_table::RegistryId(self.next())
  }

  pub fn next_artifact_id(&mut self, debug_name: String) -> symbol_table::UniverseId {
    symbol_table::UniverseId(self.next(), debug_name)
  }

  pub fn next_type_id(&mut self) -> symbol_table::TypeId {
    symbol_table::TypeId(self.next())
  }

  pub fn next_substitution_id(&mut self) -> symbol_table::SubstitutionId {
    symbol_table::SubstitutionId(self.next())
  }
}

pub enum CallGraphTraversalRecursionBehavior {
  IgnoreAndSkip,
  Follow,
  Stop,
}

#[derive(Default, Clone, Debug)]
pub struct CallGraph {
  adjacency_list: std::collections::HashMap<
    symbol_table::RegistryId,
    std::collections::HashSet<symbol_table::RegistryId>,
  >,
}

impl CallGraph {
  /// Use Tarjan's algorithm to find and collect all strongly connected
  /// components (SSCs) within the call graph.
  ///
  /// These groups may represent both direct or mutually recursive functions.
  pub fn find_strongly_connected_components(
    &self,
  ) -> Vec<std::collections::HashSet<symbol_table::RegistryId>> {
    // TODO: This only considers functions; checks for mutually recursive or directly recursive types are not implemented yet!

    let mut stack = Vec::new();
    let mut indices = std::collections::HashMap::new();
    let mut low_links = std::collections::HashMap::new();
    let mut on_stack = std::collections::HashSet::new();
    let mut strongly_connected_components = Vec::new();

    for &vertex in self.adjacency_list.keys() {
      if !indices.contains_key(&vertex) {
        self.explore_connected_component(
          vertex,
          &mut stack,
          &mut indices,
          &mut low_links,
          &mut on_stack,
          &mut strongly_connected_components,
        );
      }
    }

    strongly_connected_components
  }

  fn explore_connected_component(
    &self,
    vertex: symbol_table::RegistryId,
    stack: &mut Vec<symbol_table::RegistryId>,
    indices: &mut std::collections::HashMap<symbol_table::RegistryId, usize>,
    low_links: &mut std::collections::HashMap<symbol_table::RegistryId, usize>,
    on_stack: &mut std::collections::HashSet<symbol_table::RegistryId>,
    strongly_connected_components: &mut Vec<std::collections::HashSet<symbol_table::RegistryId>>,
  ) {
    let index = indices.len();

    indices.insert(vertex, index);
    low_links.insert(vertex, index);
    stack.push(vertex);
    on_stack.insert(vertex);

    if let Some(neighbors) = self.adjacency_list.get(&vertex) {
      for &neighbor in neighbors {
        if !indices.contains_key(&neighbor) {
          self.explore_connected_component(
            neighbor,
            stack,
            indices,
            low_links,
            on_stack,
            strongly_connected_components,
          );

          if let Some(&v) = low_links.get(&vertex) {
            if let Some(&w) = low_links.get(&neighbor) {
              low_links.insert(vertex, std::cmp::min(v, w));
            }
          }
        } else if on_stack.contains(&neighbor) {
          if let Some(&v) = low_links.get(&vertex) {
            if let Some(&w) = indices.get(&neighbor) {
              low_links.insert(vertex, std::cmp::min(v, w));
            }
          }
        }
      }
    }

    if let Some(&v) = low_links.get(&vertex) {
      if let Some(&w) = indices.get(&vertex) {
        if v == w {
          let mut scc = std::collections::HashSet::new();

          loop {
            let w = stack.pop().unwrap();

            on_stack.remove(&w);
            scc.insert(w);

            if w == vertex {
              break;
            }
          }

          strongly_connected_components.push(scc);
        }
      }
    }
  }

  pub fn add_empty_entry(&mut self, function_id: symbol_table::RegistryId) -> bool {
    if !self.adjacency_list.contains_key(&function_id) {
      self
        .adjacency_list
        .insert(function_id, std::collections::HashSet::new());

      true
    } else {
      false
    }
  }

  pub fn add_link(&mut self, from: symbol_table::RegistryId, to: symbol_table::RegistryId) {
    self
      .adjacency_list
      .entry(from)
      .and_modify(|targets| {
        targets.insert(to);
      })
      .or_insert_with(|| {
        let mut set = std::collections::HashSet::new();

        set.insert(to);

        set
      });
  }

  /// Checks the adjacency list, ensuring that all and any call site
  /// present as an edge also has an entry as a vertex, and if such vertex
  /// has no edges, it should still have an empty edge set entry on the
  /// adjacency list.
  pub fn check_whether_adjacency_list_is_closed(&self) -> bool {
    for to_set in self.adjacency_list.values() {
      // NOTE: There might not be any outgoing edges for this vertex,
      // which is completely valid. In such case, nothing will be processed
      // during this iteration.
      for to in to_set {
        if !self.adjacency_list.contains_key(to) {
          return false;
        }
      }
    }

    true
  }

  pub fn traverse(
    &self,
    start: &symbol_table::RegistryId,
    visitor: &dyn Fn(&symbol_table::RegistryId) -> bool,
    recursion_behavior: CallGraphTraversalRecursionBehavior,
  ) -> Result<(), &'static str> {
    if !self.adjacency_list.contains_key(start) {
      return Err("the starting call site is not present in the adjacency list");
    }

    let mut stack = vec![start];
    let mut seen = std::collections::HashSet::new();

    while let Some(current) = stack.pop() {
      if seen.contains(current) {
        match recursion_behavior {
          CallGraphTraversalRecursionBehavior::IgnoreAndSkip => continue,
          CallGraphTraversalRecursionBehavior::Stop => return Ok(()),
          CallGraphTraversalRecursionBehavior::Follow => {}
        }
      }

      seen.insert(current);

      // If the visitor signals not to continue the traversal anymore,
      // simply stop.
      if !visitor(current) {
        return Ok(());
      }

      // The target function of the edge might not necessarily have any outgoing
      // edges on the adjacency list.
      if let Some(edges) = self.adjacency_list.get(current) {
        stack.extend(edges);
      }
    }

    Ok(())
  }
}

pub(crate) fn join_two_opts<'a, A, B>(
  a: &'a Option<A>,
  b: &'a Option<B>,
) -> Option<(&'a A, &'a B)> {
  match (a, b) {
    (Some(a), Some(b)) => Some((a, b)),
    _ => None,
  }
}
