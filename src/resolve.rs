use std::collections::{HashMap, HashSet};

use crate::error::Error;
use crate::{ast, resolved};

pub type StaticId = usize;
pub type FunctionId = usize;
pub type StackId = usize;
pub type TransientId = usize;
pub type StringId = usize;

pub type LocalId = usize;

#[derive(Debug, Clone, Copy)]
enum LocalWip {
    Stack(StackId),
    Transient,
}

#[derive(Debug, Clone, Copy)]
pub enum Variable {
    Static(StaticId),
    Function(FunctionId),
    Local(LocalId),
}

#[derive(Debug, Clone, Copy)]
pub enum Local {
    Stack(StackId),
    Transient(TransientId),
}

#[derive(Debug, Clone)]
pub struct ModuleTree {
    statics: HashMap<String, StaticId>,
    functions: HashMap<String, FunctionId>,
    modules: HashMap<String, ModuleTree>,
}

impl ModuleTree {
    fn contains(&self, name: &str) -> bool {
        self.statics.contains_key(name)
            || self.functions.contains_key(name)
            || self.modules.contains_key(name)
    }
}

struct Counter {
    static_count: usize,
    function_count: usize,
}

impl Counter {
    fn new() -> Self {
        Counter {
            static_count: 0,
            function_count: 0,
        }
    }

    fn static_id(&mut self) -> StaticId {
        let id = self.static_count;
        self.static_count += 1;
        id
    }

    fn function_id(&mut self) -> FunctionId {
        let id = self.function_count;
        self.function_count += 1;
        id
    }
}

#[derive(Debug)]
struct GlobalContext<'a> {
    statics: HashMap<String, StaticId>,
    functions: HashMap<String, FunctionId>,
    modules: HashMap<String, &'a ModuleTree>,
}

#[derive(Debug, Clone)]
pub struct Resolved {
    pub functions: HashMap<FunctionId, resolved::Function>,
    pub statics: HashMap<StaticId, resolved::Static>,
    pub strings: HashMap<StringId, String>,
}

pub fn resolve(mut crates: HashMap<String, Vec<ast::Item>>) -> Result<Resolved, Error> {
    let tree = make_full_module_tree(&mut crates)?;
    if !tree["crate"].functions.contains_key("main") {
        return Err(Error::msg("no `main` function"));
    }

    let mut statics = HashMap::new();
    let mut functions = HashMap::new();
    let mut strings = HashMap::new();
    for (name, items) in crates {
        let mut prefix = vec![name.clone()];
        let (crate_functions, crate_statics) =
            resolve_items(&mut prefix, items, &tree[&name], &tree, &mut strings)?;
        statics.extend(crate_statics);
        functions.extend(crate_functions);
    }

    Ok(Resolved {
        functions,
        statics,
        strings: strings.into_iter().map(|(s, id)| (id, s)).collect(),
    })
}

fn resolve_items(
    prefix: &mut Vec<String>,
    items: Vec<ast::Item>,
    tree: &ModuleTree,
    full_tree: &HashMap<String, ModuleTree>,
    strings: &mut HashMap<String, StringId>,
) -> Result<
    (
        HashMap<FunctionId, resolved::Function>,
        HashMap<StaticId, resolved::Static>,
    ),
    Error,
> {
    let global_context = make_global_context(&items, tree, full_tree)?;
    let mut functions = HashMap::new();
    let mut statics = HashMap::new();

    for item in items {
        match item {
            ast::Item::Function {
                name,
                params,
                block,
                id,
            } => {
                functions.insert(
                    id.unwrap(),
                    resolve_function(
                        prefix,
                        name,
                        &params,
                        block,
                        id.unwrap(),
                        &global_context,
                        strings,
                    )?,
                );
            }
            ast::Item::Static { name, expr, id } => {
                statics.insert(
                    id.unwrap(),
                    resolve_static(prefix, name, expr, id.unwrap(), &global_context, strings)?,
                );
            }
            ast::Item::Mod { name, items } => {
                let tree = &tree.modules[&name.name];
                prefix.push(name.name.clone());
                let (mod_functions, mod_statics) =
                    resolve_items(prefix, items, tree, full_tree, strings)?;
                prefix.pop();
                functions.extend(mod_functions);
                statics.extend(mod_statics);
            }
            ast::Item::Use { .. } => {}
        }
    }

    Ok((functions, statics))
}

fn resolve_function(
    prefix: &[String],
    name: ast::Name,
    params: &[ast::Name],
    mut block: ast::Block,
    id: FunctionId,
    context: &GlobalContext,
    strings: &mut HashMap<String, StringId>,
) -> Result<resolved::Function, Error> {
    let mut resolver = Resolver::with_params(&params, context, strings)?;
    resolver.visit_block(&mut block)?;
    let params: Vec<_> = params
        .iter()
        .map(|param| resolver.convert_local_id(resolver.variable_stack[0][&param.name]))
        .collect();

    if name.name == "main" && prefix == ["crate"] && !params.is_empty() && params.len() != 2 {
        Err(Error::new(name.span, "`main` must have 0 or 2 parameters"))
    } else {
        Ok(resolved::Function {
            prefix: prefix.to_vec(),
            name: name.name,
            span: name.span,
            id,
            params,
            block: resolver.convert_block(block)?,
            transient_locals: resolver.transient_map.len(),
            stack_locals: resolver.max_stack_locals,
            static_dependencies: resolver.static_dependencies.into_iter().collect(),
            function_dependencies: resolver.function_dependencies.into_iter().collect(),
        })
    }
}

fn resolve_static(
    prefix: &[String],
    name: ast::Name,
    mut expr: ast::Expr,
    id: StaticId,
    context: &GlobalContext,
    strings: &mut HashMap<String, StringId>,
) -> Result<resolved::Static, Error> {
    let mut resolver = Resolver::new(context, strings);
    resolver.visit_expr(&mut expr)?;
    Ok(resolved::Static {
        prefix: prefix.to_vec(),
        name: name.name,
        span: name.span,
        id,
        expr: resolver.convert_expr(expr)?,
        transient_locals: resolver.transient_map.len(),
        stack_locals: resolver.max_stack_locals,
        static_dependencies: resolver.static_dependencies.into_iter().collect(),
        function_dependencies: resolver.function_dependencies.into_iter().collect(),
    })
}

fn make_full_module_tree(
    crates: &mut HashMap<String, Vec<ast::Item>>,
) -> Result<HashMap<String, ModuleTree>, Error> {
    let mut counter = Counter::new();
    let mut tree = HashMap::new();
    for (name, items) in crates {
        tree.insert(name.clone(), make_module_tree(items, &mut counter)?);
    }
    Ok(tree)
}

fn make_module_tree(items: &mut [ast::Item], counter: &mut Counter) -> Result<ModuleTree, Error> {
    let mut statics = HashMap::new();
    let mut functions = HashMap::new();
    let mut modules = HashMap::new();

    for item in items {
        match item {
            ast::Item::Function { name, id, .. } => {
                if statics.contains_key(&name.name) || functions.contains_key(&name.name) {
                    return Err(Error::new(
                        name.span,
                        format!("duplicate global `{}`", name.name),
                    ));
                }
                let function_id = counter.function_id();
                *id = Some(function_id);
                functions.insert(name.name.to_string(), function_id);
            }
            ast::Item::Static { name, id, .. } => {
                if statics.contains_key(&name.name) || functions.contains_key(&name.name) {
                    return Err(Error::new(
                        name.span,
                        format!("duplicate global `{}`", name.name),
                    ));
                }
                let static_id = counter.static_id();
                *id = Some(static_id);
                statics.insert(name.name.to_string(), static_id);
            }
            ast::Item::Mod { name, items } => {
                let tree = make_module_tree(items, counter)?;
                modules.insert(name.name.to_string(), tree);
            }
            ast::Item::Use { .. } => {}
        };
    }

    Ok(ModuleTree {
        statics,
        functions,
        modules,
    })
}

fn make_global_context<'a>(
    items: &[ast::Item],
    local_tree: &'a ModuleTree,
    full_tree: &'a HashMap<String, ModuleTree>,
) -> Result<GlobalContext<'a>, Error> {
    let mut statics = local_tree.statics.clone();
    let mut functions = local_tree.functions.clone();
    let mut modules: HashMap<_, _> = full_tree
        .iter()
        .map(|(name, tree)| (name.to_string(), tree))
        .collect();

    for item in items {
        if let ast::Item::Mod { name, .. } = item {
            if modules.contains_key(&name.name) {
                return Err(Error::new(
                    name.span,
                    format!("duplicate module `{}`", name.name),
                ));
            }
            modules.insert(name.name.to_string(), &local_tree.modules[&name.name]);
        }
    }

    loop {
        let mut first_unresolved_use = None;
        let mut resolved_this_round = false;

        for item in items {
            if let ast::Item::Use {
                with_crate,
                tree,
                done,
            } = item
            {
                if done.get() {
                    continue;
                }

                let module = if *with_crate {
                    &full_tree["crate"]
                } else if let Some(module) = modules.get(&tree.prefix[0].name) {
                    module
                } else {
                    if first_unresolved_use.is_none() {
                        first_unresolved_use = Some(tree.prefix[0].span);
                    }
                    continue;
                };

                let skip = usize::from(!*with_crate);
                update_context(
                    tree,
                    module,
                    skip,
                    &mut statics,
                    &mut functions,
                    &mut modules,
                )?;

                resolved_this_round = true;
                done.set(true);
            }
        }

        if let Some(unresolved_use) = first_unresolved_use {
            if !resolved_this_round {
                return Err(Error::new(
                    unresolved_use,
                    format!("no module `{}`", unresolved_use.text),
                ));
            }
        } else {
            break;
        }
    }

    Ok(GlobalContext {
        statics,
        functions,
        modules,
    })
}

fn update_context<'a>(
    tree: &ast::UseTree,
    mut module: &'a ModuleTree,
    skip: usize,
    statics: &mut HashMap<String, StaticId>,
    functions: &mut HashMap<String, FunctionId>,
    modules: &mut HashMap<String, &'a ModuleTree>,
) -> Result<(), Error> {
    match &tree.kind {
        ast::UseTreeKind::Simple => {
            for (i, name) in tree.prefix.iter().enumerate().skip(skip) {
                if i == tree.prefix.len() - 1 {
                    if !module.contains(&name.name) {
                        return Err(Error::new(
                            name.span,
                            format!("no `{}` in module", name.name),
                        ));
                    }
                    if let Some(static_id) = module.statics.get(&name.name) {
                        if statics.contains_key(&name.name) || functions.contains_key(&name.name) {
                            return Err(Error::new(
                                name.span,
                                format!("duplicate global `{}`", name.name),
                            ));
                        }
                        statics.insert(name.name.to_string(), *static_id);
                    } else if let Some(function_id) = module.functions.get(&name.name) {
                        if statics.contains_key(&name.name) || functions.contains_key(&name.name) {
                            return Err(Error::new(
                                name.span,
                                format!("duplicate global `{}`", name.name),
                            ));
                        }
                        functions.insert(name.name.to_string(), *function_id);
                    }
                    if let Some(module) = module.modules.get(&name.name) {
                        if modules.contains_key(&name.name) {
                            return Err(Error::new(
                                name.span,
                                format!("duplicate module `{}`", name.name),
                            ));
                        }
                        modules.insert(name.name.to_string(), module);
                    }
                } else {
                    module = module.modules.get(&name.name).ok_or_else(|| {
                        Error::new(name.span, format!("no module `{}`", name.name))
                    })?;
                }
            }
        }
        ast::UseTreeKind::Nested(nested) => {
            for name in tree.prefix.iter().skip(skip) {
                module = module
                    .modules
                    .get(&name.name)
                    .ok_or_else(|| Error::new(name.span, format!("no module `{}`", name.name)))?;
            }

            for tree in nested {
                update_context(tree, module, 0, statics, functions, modules)?;
            }
        }
    }
    Ok(())
}

struct Resolver<'a> {
    context: &'a GlobalContext<'a>,
    strings: &'a mut HashMap<String, StringId>,
    variable_stack: Vec<HashMap<String, LocalId>>,
    local_map: HashMap<LocalId, LocalWip>,
    local_counter: usize,
    current_stack_locals: usize,
    max_stack_locals: usize,
    transient_map: HashMap<LocalId, TransientId>,
    static_dependencies: HashSet<StaticId>,
    function_dependencies: HashSet<FunctionId>,
}

impl<'a> Resolver<'a> {
    fn new(context: &'a GlobalContext<'a>, strings: &'a mut HashMap<String, StringId>) -> Self {
        Resolver {
            context,
            strings,
            variable_stack: Vec::new(),
            local_map: HashMap::new(),
            local_counter: 0,
            current_stack_locals: 0,
            max_stack_locals: 0,
            transient_map: HashMap::new(),
            static_dependencies: HashSet::new(),
            function_dependencies: HashSet::new(),
        }
    }

    fn with_params(
        params: &[ast::Name],
        context: &'a GlobalContext<'a>,
        strings: &'a mut HashMap<String, StringId>,
    ) -> Result<Self, Error> {
        let mut param_map = HashMap::new();
        for (local_id, param) in params.iter().enumerate() {
            if param_map.contains_key(&param.name) {
                return Err(Error::new(
                    param.span,
                    format!("duplicate parameter `{}`", param.name),
                ));
            }
            param_map.insert(param.name.clone(), local_id);
        }

        Ok(Resolver {
            context,
            strings,
            variable_stack: vec![param_map],
            local_map: (0..params.len())
                .map(|id| (id, LocalWip::Transient))
                .collect(),
            local_counter: params.len(),
            current_stack_locals: 0,
            max_stack_locals: 0,
            transient_map: HashMap::new(),
            static_dependencies: HashSet::new(),
            function_dependencies: HashSet::new(),
        })
    }

    fn convert_local_id(&mut self, local_id: LocalId) -> Local {
        match self.local_map[&local_id] {
            LocalWip::Stack(id) => Local::Stack(id),
            LocalWip::Transient => {
                if let Some(transient_id) = self.transient_map.get(&local_id) {
                    Local::Transient(*transient_id)
                } else {
                    let transient_id = self.transient_map.len();
                    self.transient_map.insert(local_id, transient_id);
                    Local::Transient(transient_id)
                }
            }
        }
    }

    fn convert_local_id_to_assign_expr(&mut self, local_id: LocalId) -> resolved::AssignTargetExpr {
        let id = self.convert_local_id(local_id);
        match id {
            Local::Stack(stack_id) => resolved::AssignTargetExpr::Stack(stack_id),
            Local::Transient(transient_id) => resolved::AssignTargetExpr::Transient(transient_id),
        }
    }

    fn resolve(
        &mut self,
        with_crate: bool,
        prefix: &[ast::Name],
        name: &ast::Name,
    ) -> Result<Variable, Error> {
        if prefix.is_empty() {
            self.resolve_simple(name)
        } else {
            self.resolve_qualified(with_crate, prefix, name)
        }
    }

    fn resolve_simple(&mut self, name: &ast::Name) -> Result<Variable, Error> {
        for frame in self.variable_stack.iter().rev() {
            if let Some(id) = frame.get(&name.name) {
                return Ok(Variable::Local(*id));
            }
        }

        if let Some(static_id) = self.context.statics.get(&name.name) {
            self.static_dependencies.insert(*static_id);
            Ok(Variable::Static(*static_id))
        } else if let Some(function_id) = self.context.functions.get(&name.name) {
            self.function_dependencies.insert(*function_id);
            Ok(Variable::Function(*function_id))
        } else {
            Err(Error::new(
                name.span,
                format!("no variable `{}`", name.name),
            ))
        }
    }

    fn resolve_qualified(
        &mut self,
        with_crate: bool,
        prefix: &[ast::Name],
        name: &ast::Name,
    ) -> Result<Variable, Error> {
        let mut module = if with_crate {
            self.context.modules["crate"]
        } else {
            self.context
                .modules
                .get(&prefix[0].name)
                .ok_or_else(|| Error::new(prefix[0].span, format!("no module `{}`", name.name)))?
        };

        let skip = usize::from(!with_crate);
        for name in prefix.iter().skip(skip) {
            module = module
                .modules
                .get(&name.name)
                .ok_or_else(|| Error::new(name.span, format!("no module `{}`", name.name)))?;
        }

        if let Some(static_id) = module.statics.get(&name.name) {
            self.static_dependencies.insert(*static_id);
            Ok(Variable::Static(*static_id))
        } else if let Some(function_id) = module.functions.get(&name.name) {
            self.function_dependencies.insert(*function_id);
            Ok(Variable::Function(*function_id))
        } else {
            Err(Error::new(
                name.span,
                format!("no variable `{}`", name.name),
            ))
        }
    }

    fn define(&mut self, name: &ast::Name) -> LocalId {
        let local_id = self.local_counter;
        self.local_counter += 1;
        self.variable_stack
            .last_mut()
            .unwrap()
            .insert(name.name.clone(), local_id);
        self.local_map.insert(local_id, LocalWip::Transient);
        local_id
    }

    fn ensure_on_stack(&mut self, local_id: LocalId) {
        if matches!(self.local_map.get(&local_id).unwrap(), LocalWip::Transient) {
            let stack_id = self.current_stack_locals;
            self.current_stack_locals += 1;
            if self.current_stack_locals > self.max_stack_locals {
                self.max_stack_locals = self.current_stack_locals;
            }
            self.local_map.insert(local_id, LocalWip::Stack(stack_id));
        }
    }

    fn enter_block(&mut self) {
        self.variable_stack.push(HashMap::new());
    }

    fn exit_block(&mut self) {
        for local_id in self.variable_stack.pop().unwrap().into_values() {
            if matches!(self.local_map.get(&local_id).unwrap(), LocalWip::Stack(_)) {
                self.current_stack_locals -= 1;
            }
        }
    }

    fn visit_block(&mut self, block: &mut ast::Block) -> Result<(), Error> {
        self.enter_block();
        for stmt in &mut block.stmts {
            match stmt {
                ast::Stmt::Let {
                    name,
                    expr,
                    local_id,
                } => {
                    self.visit_expr(expr)?;
                    *local_id = Some(self.define(name));
                }
                ast::Stmt::Expr(expr) => self.visit_expr(expr)?,
            }
        }
        if let Some(expr) = &mut block.expr {
            self.visit_expr(expr)?;
        }
        self.exit_block();
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut ast::Expr) -> Result<(), Error> {
        match expr {
            ast::Expr::String(_) | ast::Expr::Int(_) => {}
            ast::Expr::Path {
                with_crate,
                prefix,
                name,
                variable,
            } => *variable = Some(self.resolve(*with_crate, prefix, name)?),
            ast::Expr::Block(block) => self.visit_block(block)?,
            ast::Expr::AddrOf(expr) => {
                if let ast::PlaceExpr::Path {
                    with_crate,
                    prefix,
                    name,
                    ..
                } = expr
                {
                    if let Variable::Local(id) = self.resolve(*with_crate, prefix, name)? {
                        self.ensure_on_stack(id);
                    }
                }
                self.visit_place_expr(expr)?;
            }
            ast::Expr::Binary { lhs, rhs, .. } => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)?;
            }
            ast::Expr::Unary { expr, .. } => self.visit_expr(expr)?,
            ast::Expr::Assign { target, rhs } | ast::Expr::AssignOp { target, rhs, .. } => {
                self.visit_place_expr(target)?;
                self.visit_expr(rhs)?;
            }
            ast::Expr::Index { target, index } => {
                self.visit_expr(target)?;
                self.visit_expr(index)?;
            }
            ast::Expr::Call { func, args } => {
                self.visit_expr(func)?;
                for arg in args {
                    self.visit_expr(arg)?;
                }
            }
            ast::Expr::If {
                test,
                if_block,
                else_block,
            } => {
                self.visit_expr(test)?;
                self.visit_block(if_block)?;
                if let Some(else_block) = else_block {
                    self.visit_block(else_block)?;
                }
            }
            ast::Expr::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr)?;
                }
            }
            ast::Expr::For {
                init,
                test,
                update,
                block,
            } => {
                self.enter_block();
                match init {
                    Some(ast::ForInit::Let {
                        name,
                        expr,
                        local_id,
                    }) => {
                        self.visit_expr(expr)?;
                        *local_id = Some(self.define(name));
                    }
                    Some(ast::ForInit::Expr(expr)) => self.visit_expr(expr)?,
                    None => {}
                }
                if let Some(test) = test {
                    self.visit_expr(test)?;
                }
                if let Some(update) = update {
                    self.visit_expr(update)?;
                }
                self.visit_block(block)?;
                self.exit_block();
            }
        }
        Ok(())
    }

    fn visit_place_expr(&mut self, expr: &mut ast::PlaceExpr) -> Result<(), Error> {
        match expr {
            ast::PlaceExpr::Path {
                with_crate,
                prefix,
                name,
                variable,
            } => *variable = Some(self.resolve(*with_crate, prefix, name)?),
            ast::PlaceExpr::Deref(expr) => self.visit_expr(expr)?,
            ast::PlaceExpr::Index { target, index } => {
                self.visit_expr(target)?;
                self.visit_expr(index)?;
            }
        }
        Ok(())
    }

    fn convert_block(&mut self, block: ast::Block) -> Result<resolved::Block, Error> {
        let mut stmts = Vec::new();
        for stmt in block.stmts {
            let converted_stmt = match stmt {
                ast::Stmt::Let { expr, local_id, .. } => resolved::Expr::Assign {
                    target: self.convert_local_id_to_assign_expr(local_id.unwrap()),
                    rhs: Box::new(self.convert_expr(expr)?),
                },
                ast::Stmt::Expr(expr) => self.convert_expr(expr)?,
            };
            stmts.push(converted_stmt);
        }
        let expr = if let Some(expr) = block.expr {
            Some(Box::new(self.convert_expr(*expr)?))
        } else {
            None
        };
        Ok(resolved::Block { stmts, expr })
    }

    fn convert_expr(&mut self, expr: ast::Expr) -> Result<resolved::Expr, Error> {
        match expr {
            ast::Expr::String(string) => {
                if let Some(id) = self.strings.get(&string) {
                    Ok(resolved::Expr::String(*id))
                } else {
                    let id = self.strings.len();
                    self.strings.insert(string, id);
                    Ok(resolved::Expr::String(id))
                }
            }
            ast::Expr::Int(int) => Ok(resolved::Expr::Int(int)),
            ast::Expr::Path { variable, .. } => match variable.unwrap() {
                Variable::Static(id) => Ok(resolved::Expr::Static(id)),
                Variable::Function(id) => Ok(resolved::Expr::Function(id)),
                Variable::Local(id) => match self.convert_local_id(id) {
                    Local::Stack(id) => Ok(resolved::Expr::Stack(id)),
                    Local::Transient(id) => Ok(resolved::Expr::Transient(id)),
                },
            },
            ast::Expr::Block(block) => Ok(resolved::Expr::Block(self.convert_block(block)?)),
            ast::Expr::AddrOf(expr) => Ok(resolved::Expr::AddrOf(self.convert_addr_of_expr(expr)?)),
            ast::Expr::Binary { op, lhs, rhs } => Ok(resolved::Expr::Binary {
                op,
                lhs: Box::new(self.convert_expr(*lhs)?),
                rhs: Box::new(self.convert_expr(*rhs)?),
            }),
            ast::Expr::Unary { op, expr } => Ok(resolved::Expr::Unary {
                op,
                expr: Box::new(self.convert_expr(*expr)?),
            }),
            ast::Expr::Assign { target, rhs } => Ok(resolved::Expr::Assign {
                target: self.convert_assign_target_expr(target)?,
                rhs: Box::new(self.convert_expr(*rhs)?),
            }),
            ast::Expr::AssignOp { op, target, rhs } => Ok(resolved::Expr::AssignOp {
                op,
                target: self.convert_assign_target_expr(target)?,
                rhs: Box::new(self.convert_expr(*rhs)?),
            }),
            ast::Expr::Index { target, index } => Ok(resolved::Expr::Index {
                target: Box::new(self.convert_expr(*target)?),
                index: Box::new(self.convert_expr(*index)?),
            }),
            ast::Expr::Call { func, args } => Ok(resolved::Expr::Call {
                func: Box::new(self.convert_expr(*func)?),
                args: args
                    .into_iter()
                    .map(|arg| self.convert_expr(arg))
                    .collect::<Result<_, _>>()?,
            }),
            ast::Expr::If {
                test,
                if_block,
                else_block,
            } => Ok(resolved::Expr::If {
                test: Box::new(self.convert_expr(*test)?),
                if_block: self.convert_block(if_block)?,
                else_block: if let Some(else_block) = else_block {
                    Some(self.convert_block(else_block)?)
                } else {
                    None
                },
            }),
            ast::Expr::Return(expr) => {
                if let Some(expr) = expr {
                    Ok(resolved::Expr::Return(Some(Box::new(
                        self.convert_expr(*expr)?,
                    ))))
                } else {
                    Ok(resolved::Expr::Return(None))
                }
            }
            ast::Expr::For {
                init,
                test,
                update,
                block,
            } => {
                let init = match init {
                    Some(ast::ForInit::Let { expr, local_id, .. }) => {
                        Some(Box::new(resolved::Expr::Assign {
                            target: self.convert_local_id_to_assign_expr(local_id.unwrap()),
                            rhs: Box::new(self.convert_expr(*expr)?),
                        }))
                    }
                    Some(ast::ForInit::Expr(expr)) => Some(Box::new(self.convert_expr(*expr)?)),
                    None => None,
                };
                let test = if let Some(test) = test {
                    Some(Box::new(self.convert_expr(*test)?))
                } else {
                    None
                };
                let update = if let Some(update) = update {
                    Some(Box::new(self.convert_expr(*update)?))
                } else {
                    None
                };
                let block = self.convert_block(block)?;
                Ok(resolved::Expr::For {
                    init,
                    test,
                    update,
                    block,
                })
            }
        }
    }

    fn convert_assign_target_expr(
        &mut self,
        expr: ast::PlaceExpr,
    ) -> Result<resolved::AssignTargetExpr, Error> {
        match expr {
            ast::PlaceExpr::Path { name, variable, .. } => {
                match variable.unwrap() {
                    Variable::Static(id) => Ok(resolved::AssignTargetExpr::Static(id)),
                    Variable::Function(_) => Err(Error::new(
                        name.span,
                        format!("`{}` is a function", name.name), // FIXME
                    )),
                    Variable::Local(id) => match self.convert_local_id(id) {
                        Local::Stack(id) => Ok(resolved::AssignTargetExpr::Stack(id)),
                        Local::Transient(id) => Ok(resolved::AssignTargetExpr::Transient(id)),
                    },
                }
            }

            ast::PlaceExpr::Deref(expr) => Ok(resolved::AssignTargetExpr::Deref(Box::new(
                self.convert_expr(*expr)?,
            ))),
            ast::PlaceExpr::Index { target, index } => Ok(resolved::AssignTargetExpr::Index {
                target: Box::new(self.convert_expr(*target)?),
                index: Box::new(self.convert_expr(*index)?),
            }),
        }
    }

    fn convert_addr_of_expr(
        &mut self,
        expr: ast::PlaceExpr,
    ) -> Result<resolved::AddrOfExpr, Error> {
        match expr {
            ast::PlaceExpr::Path { name, variable, .. } => match variable.unwrap() {
                Variable::Static(id) => Ok(resolved::AddrOfExpr::Static(id)),
                Variable::Function(_) => Err(Error::new(
                    name.span,
                    format!("`{}` is a function", name.name),
                )),
                Variable::Local(id) => match self.convert_local_id(id) {
                    Local::Stack(id) => Ok(resolved::AddrOfExpr::Stack(id)),
                    Local::Transient(_) => unreachable!(),
                },
            },
            ast::PlaceExpr::Deref(_) => unreachable!(),
            ast::PlaceExpr::Index { target, index } => Ok(resolved::AddrOfExpr::Index {
                target: Box::new(self.convert_expr(*target)?),
                index: Box::new(self.convert_expr(*index)?),
            }),
        }
    }
}
