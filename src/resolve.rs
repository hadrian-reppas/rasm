use std::collections::{HashMap, HashSet};

use crate::builtins::BUILTIN_FUNCTIONS;
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

#[derive(Debug, Clone)]
pub struct Resolved {
    pub functions: Vec<resolved::Function>,
    pub statics: Vec<resolved::Static>,
    pub strings: Vec<String>,
}

pub fn resolve(items: Vec<ast::Item>) -> Result<Resolved, Error> {
    let (static_names, function_names) = make_statics_and_functions(&items)?;
    let mut statics = Vec::new();
    let mut functions = Vec::new();
    let mut strings = HashMap::new();

    for item in items {
        match resolve_item(item, &static_names, &function_names, &mut strings)? {
            resolved::Item::Static(static_) => statics.push(static_),
            resolved::Item::Function(function) => functions.push(function),
        }
    }

    let mut strings: Vec<_> = strings.into_iter().collect();
    strings.sort_by_key(|(_, id)| *id);

    if function_names.contains_key("main") {
        Ok(Resolved {
            functions,
            statics,
            strings: strings.into_iter().map(|(s, _)| s).collect(),
        })
    } else {
        Err(Error::msg("no `main` function"))
    }
}

fn make_statics_and_functions(
    items: &[ast::Item],
) -> Result<(HashMap<String, StaticId>, HashMap<String, FunctionId>), Error> {
    let mut statics = HashMap::new();
    let mut functions: HashMap<_, _> = BUILTIN_FUNCTIONS
        .iter()
        .map(|func| func.name.to_string())
        .zip(0..)
        .collect();

    for item in items {
        let (name, is_function) = match item {
            ast::Item::Function { name, .. } => (name, true),
            ast::Item::Static { name, .. } => (name, false),
            ast::Item::Mod { .. } => todo!(),
            ast::Item::Use { .. } => todo!(),
        };

        if statics.contains_key(&name.name) || functions.contains_key(&name.name) {
            return Err(Error::new(
                name.span,
                format!("duplicate global `{}`", name.name),
            ));
        }

        if is_function {
            functions.insert(name.name.to_string(), functions.len());
        } else {
            statics.insert(name.name.to_string(), statics.len());
        }
    }
    Ok((statics, functions))
}

fn resolve_item(
    item: ast::Item,
    statics: &HashMap<String, StaticId>,
    functions: &HashMap<String, FunctionId>,
    strings: &mut HashMap<String, StringId>,
) -> Result<resolved::Item, Error> {
    match item {
        ast::Item::Function {
            name,
            params,
            mut block,
        } => {
            let mut resolver = Resolver::with_params(&params, statics, functions, strings)?;
            resolver.visit_block(&mut block)?;
            let params: Vec<_> = params
                .iter()
                .map(|param| resolver.convert_local_id(resolver.variable_stack[0][&param.name]))
                .collect();

            if name.name == "main" && !params.is_empty() && params.len() != 2 {
                Err(Error::new(name.span, "`main` must have 0 or 2 parameters"))
            } else {
                Ok(resolved::Item::Function(resolved::Function {
                    name: name.name.to_string(),
                    span: name.span,
                    id: functions[&name.name],
                    params,
                    block: resolver.convert_block(block)?,
                    transient_locals: resolver.transient_map.len(),
                    stack_locals: resolver.max_stack_locals,
                    static_dependencies: resolver.static_dependencies.into_iter().collect(),
                    function_dependencies: resolver.function_dependencies.into_iter().collect(),
                }))
            }
        }
        ast::Item::Static { name, mut expr } => {
            let mut resolver = Resolver::new(statics, functions, strings);
            resolver.visit_expr(&mut expr)?;
            Ok(resolved::Item::Static(resolved::Static {
                name: name.name.to_string(),
                span: name.span,
                id: statics[&name.name],
                expr: resolver.convert_expr(expr)?,
                transient_locals: resolver.transient_map.len(),
                stack_locals: resolver.max_stack_locals,
                static_dependencies: resolver.static_dependencies.into_iter().collect(),
                function_dependencies: resolver.function_dependencies.into_iter().collect(),
            }))
        }
        ast::Item::Mod { .. } => todo!(),
        ast::Item::Use { .. } => todo!(),
    }
}

struct Resolver<'a> {
    statics: &'a HashMap<String, StaticId>,
    functions: &'a HashMap<String, FunctionId>,
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
    fn new(
        statics: &'a HashMap<String, StaticId>,
        functions: &'a HashMap<String, FunctionId>,
        strings: &'a mut HashMap<String, StringId>,
    ) -> Self {
        Resolver {
            statics,
            functions,
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
        statics: &'a HashMap<String, StaticId>,
        functions: &'a HashMap<String, FunctionId>,
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
            statics,
            functions,
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

    fn resolve(&mut self, name: &ast::Name) -> Result<Variable, Error> {
        for frame in self.variable_stack.iter().rev() {
            if let Some(id) = frame.get(&name.name) {
                return Ok(Variable::Local(*id));
            }
        }

        if let Some(static_id) = self.statics.get(&name.name) {
            self.static_dependencies.insert(*static_id);
            Ok(Variable::Static(*static_id))
        } else if let Some(function_id) = self.functions.get(&name.name) {
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
            ast::Expr::String(_) | ast::Expr::Int(_) => Ok(()),
            ast::Expr::Path {
                with_crate,
                prefix,
                name,
            } => self.visit_name(name), // FIXME
            ast::Expr::Block(block) => self.visit_block(block),
            ast::Expr::AddrOf(expr) => {
                if let ast::PlaceExpr::Path {
                    with_crate,
                    prefix,
                    name,
                } = expr
                {
                    // FIXME
                    if let Variable::Local(id) = self.resolve(name)? {
                        self.ensure_on_stack(id);
                    }
                }
                self.visit_place_expr(expr)
            }
            ast::Expr::Binary { lhs, rhs, .. } => {
                self.visit_expr(lhs)?;
                self.visit_expr(rhs)
            }
            ast::Expr::Unary { expr, .. } => self.visit_expr(expr),
            ast::Expr::Assign { target, rhs } | ast::Expr::AssignOp { target, rhs, .. } => {
                self.visit_place_expr(target)?;
                self.visit_expr(rhs)
            }
            ast::Expr::Index { target, index } => {
                self.visit_expr(target)?;
                self.visit_expr(index)
            }
            ast::Expr::Call { func, args } => {
                self.visit_expr(func)?;
                for arg in args {
                    self.visit_expr(arg)?;
                }
                Ok(())
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
                Ok(())
            }
            ast::Expr::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr)
                } else {
                    Ok(())
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
                Ok(())
            }
        }
    }

    fn visit_name(&mut self, name: &mut ast::Name) -> Result<(), Error> {
        name.variable_id = Some(self.resolve(name)?);
        Ok(())
    }

    fn visit_place_expr(&mut self, expr: &mut ast::PlaceExpr) -> Result<(), Error> {
        match expr {
            ast::PlaceExpr::Path {
                with_crate,
                prefix,
                name,
            } => self.visit_name(name), // FIXME
            ast::PlaceExpr::Deref(expr) => self.visit_expr(expr),
            ast::PlaceExpr::Index { target, index } => {
                self.visit_expr(target)?;
                self.visit_expr(index)
            }
        }
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
            ast::Expr::Path {
                with_crate,
                prefix,
                name,
            } => {
                // FIXME
                match name.variable_id.unwrap() {
                    Variable::Static(id) => Ok(resolved::Expr::Static(id)),
                    Variable::Function(id) => Ok(resolved::Expr::Function(id)),
                    Variable::Local(id) => match self.convert_local_id(id) {
                        Local::Stack(id) => Ok(resolved::Expr::Stack(id)),
                        Local::Transient(id) => Ok(resolved::Expr::Transient(id)),
                    },
                }
            }
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
            ast::PlaceExpr::Path {
                with_crate,
                prefix,
                name,
            } => {
                // FIXME
                match name.variable_id.unwrap() {
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
            ast::PlaceExpr::Path {
                with_crate,
                prefix,
                name,
            } => {
                // FIXME
                match name.variable_id.unwrap() {
                    Variable::Static(id) => Ok(resolved::AddrOfExpr::Static(id)),
                    Variable::Function(_) => Err(Error::new(
                        name.span,
                        format!("`{}` is a function", name.name), // FIXME
                    )),
                    Variable::Local(id) => match self.convert_local_id(id) {
                        Local::Stack(id) => Ok(resolved::AddrOfExpr::Stack(id)),
                        Local::Transient(_) => unreachable!(),
                    },
                }
            }
            ast::PlaceExpr::Deref(_) => unreachable!(),
            ast::PlaceExpr::Index { target, index } => Ok(resolved::AddrOfExpr::Index {
                target: Box::new(self.convert_expr(*target)?),
                index: Box::new(self.convert_expr(*index)?),
            }),
        }
    }
}
