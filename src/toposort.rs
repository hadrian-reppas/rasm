use std::collections::HashSet;

use crate::builtins::BUILTIN_FUNCTIONS;
use crate::error::Error;
use crate::resolve::{Resolved, StaticId};

pub fn static_initialization_order(resolved: &Resolved) -> Result<Vec<StaticId>, Error> {
    let mut static_dependencies = vec![Vec::new(); resolved.statics.len()];

    for static_ in &resolved.statics {
        let mut dependencies: HashSet<_> = static_.static_dependencies.iter().copied().collect();
        let mut function_stack = static_.function_dependencies.clone();
        let mut seen_functions: HashSet<_> =
            static_.function_dependencies.iter().copied().collect();

        while let Some(function_id) = function_stack.pop() {
            if function_id < BUILTIN_FUNCTIONS.len() {
                continue;
            }

            dependencies.extend(
                resolved.functions[function_id - BUILTIN_FUNCTIONS.len()]
                    .static_dependencies
                    .iter()
                    .copied(),
            );

            for id in
                &resolved.functions[function_id - BUILTIN_FUNCTIONS.len()].function_dependencies
            {
                if !seen_functions.contains(id) {
                    function_stack.push(*id);
                    seen_functions.insert(*id);
                }
            }
        }

        static_dependencies[static_.id] = dependencies.into_iter().collect();
    }

    toposort(&static_dependencies)
        .ok_or_else(|| Error::msg("cycle detected during static initialization"))
}

fn toposort(graph: &[Vec<StaticId>]) -> Option<Vec<StaticId>> {
    let mut indegrees = vec![0; graph.len()];
    for edges in graph {
        for to in edges {
            indegrees[*to] += 1;
        }
    }

    let mut roots: Vec<_> = indegrees
        .iter()
        .copied()
        .enumerate()
        .filter(|(_, deg)| *deg == 0)
        .map(|(id, _)| id)
        .collect();

    let mut order = Vec::new();
    while order.len() < graph.len() {
        let node = roots.pop()?;
        order.push(node);
        for to in &graph[node] {
            indegrees[*to] -= 1;
            if indegrees[*to] == 0 {
                roots.push(*to);
            }
        }
    }

    order.reverse();
    Some(order)
}
