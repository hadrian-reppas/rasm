use std::collections::HashSet;

use crate::error::{Error, Span};
use crate::resolve::{GlobalId, Resolved};

pub fn global_initialization_order(resolved: &Resolved) -> Result<Vec<GlobalId>, Error> {
    let mut global_dependencies = vec![Vec::new(); resolved.globals.len()];

    for global in &resolved.globals {
        let mut dependencies: HashSet<_> = global.global_dependencies.iter().copied().collect();
        let mut function_stack = global.function_dependencies.clone();
        let mut seen_functions: HashSet<_> = global.function_dependencies.iter().copied().collect();

        while let Some(function_id) = function_stack.pop() {
            dependencies.extend(
                resolved.functions[function_id as usize]
                    .global_dependencies
                    .iter()
                    .copied(),
            );

            for id in &resolved.functions[function_id as usize].function_dependencies {
                if !seen_functions.contains(id) {
                    function_stack.push(*id);
                    seen_functions.insert(*id);
                }
            }
        }

        global_dependencies[global.id as usize] = dependencies.into_iter().collect();
    }

    toposort(&global_dependencies).ok_or(Error {
        msg: "cycle detected during global initialization".to_string(),
        span: Span::empty(),
    })
}

fn toposort(graph: &[Vec<GlobalId>]) -> Option<Vec<GlobalId>> {
    let mut indegrees = vec![0; graph.len()];
    for edges in graph {
        for to in edges {
            indegrees[*to as usize] += 1;
        }
    }

    let mut roots: Vec<_> = indegrees
        .iter()
        .copied()
        .enumerate()
        .filter(|(_, deg)| *deg == 0)
        .map(|(id, _)| id as GlobalId)
        .collect();

    let mut order = Vec::new();
    while order.len() < graph.len() {
        let node = roots.pop()?;
        order.push(node);
        for to in &graph[node as usize] {
            indegrees[*to as usize] -= 1;
            if indegrees[*to as usize] == 0 {
                roots.push(*to as GlobalId);
            }
        }
    }

    order.reverse();
    Some(order)
}
