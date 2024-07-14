use std::collections::{HashMap, HashSet};

use crate::ast;
use crate::ir::{
    BasicBlock, BinaryOp, BlockId, CmpOp, Function, Instr, Terminator, UnaryOp, ValueId,
};
use crate::resolve::{FunctionId, Local, StackId, StaticId, StringId, TransientId};
use crate::resolved::{self, AddrOfExpr, AssignTargetExpr, Block, ElseIf, Expr, ForInit, Stmt};

const SIZEOF_INT: i64 = 8;

pub fn lower(function: &resolved::Function) -> Function {
    let mut builder = FunctionBuilder::new(&function.params, function.stack_locals);
    let value = builder.block(&function.block);
    builder.return_(value);
    builder.finish()
}

pub struct FunctionBuilder {
    function_params: usize,
    stack_variables: usize,
    blocks: Vec<BasicBlockBuilder>,
    value_counter: usize,
    current_block: BlockId,
}

impl FunctionBuilder {
    fn new(params: &[Local], stack_variables: usize) -> Self {
        let entry_block = BasicBlockBuilder {
            predecessors: Vec::new(),
            params: (0..params.len()).collect(),
            instrs: Vec::new(),
            terminator: None,
            variables: HashMap::new(),
            incomplete: HashMap::new(),
            is_sealed: true,
        };

        let mut builder = FunctionBuilder {
            function_params: params.len(),
            stack_variables,
            blocks: vec![entry_block],
            value_counter: params.len(),
            current_block: 0,
        };

        for (value, param) in params.iter().enumerate() {
            match param {
                Local::Stack(slot) => builder.stack_store(*slot, value),
                Local::Transient(variable) => builder.def_variable(*variable, value),
            }
        }

        builder
    }

    fn finish(mut self) -> Function {
        // self.remove_redundant_block_params();
        self.remove_unused_block_params();
        Function {
            function_params: self.function_params,
            stack_variables: self.stack_variables,
            blocks: self
                .blocks
                .into_iter()
                .map(BasicBlockBuilder::finish)
                .collect(),
        }
    }

    fn remove_redundant_block_params(&mut self) {
        let mut map = HashMap::new();
        for block in self.blocks.iter().skip(1) {
            for (i, param) in block.params.iter().enumerate() {
                let pred_vals: HashSet<_> = block
                    .predecessors
                    .iter()
                    .map(|pred| self.get_pred_val(pred, i))
                    .filter(|val| val != param)
                    .collect();
                map.insert(*param, pred_vals);
            }
        }

        //        let mut roots = HashMap::new();
        for param in map.keys() {
            //  let mut param_roots = HashSet::new();
            let mut stack = vec![*param];
            let mut seen = HashSet::new();
            while let Some(value) = stack.pop() {
                if !seen.insert(value) {
                    continue;
                }
                // todo!();
            }
        }

        dbg!(&map);

        // TODO
    }

    fn find_and_replace_values(&mut self, map: &HashMap<ValueId, ValueId>) -> HashSet<ValueId> {
        todo!()
    }

    fn remove_unused_block_params(&mut self) {
        // TODO
        // todo!()
    }

    fn get_pred_val(&self, pred: &Predecessor, index: usize) -> ValueId {
        let terminator = self.blocks[pred.block].terminator.as_ref().unwrap();
        match pred.kind {
            PredecessorKind::Jump => terminator.get_jump_value(index),
            PredecessorKind::BranchIf => terminator.get_branch_if_value(index),
            PredecessorKind::BranchElse => terminator.get_branch_else_value(index),
        }
    }

    fn block(&mut self, block: &Block) -> ValueId {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Let { id, expr } => {
                    let value = self.expr(expr);
                    match id {
                        Local::Stack(stack_id) => self.stack_store(*stack_id, value),
                        Local::Transient(variable_id) => self.def_variable(*variable_id, value),
                    }
                }
                Stmt::Expr(expr) => {
                    self.expr(expr);
                }
            }
        }

        if let Some(expr) = &block.expr {
            self.expr(expr)
        } else {
            self.const_(0)
        }
    }

    fn expr(&mut self, expr: &Expr) -> ValueId {
        match expr {
            Expr::String(id) => self.string(*id),
            Expr::Int(i) => self.const_(*i),
            Expr::Static(id) => self.static_load(*id),
            Expr::Function(id) => self.function(*id),
            Expr::Stack(id) => self.stack_load(*id),
            Expr::Transient(id) => self.use_variable(*id),
            Expr::Block(block) => self.block(block),
            Expr::AddrOf(expr) => self.addr_of_expr(expr),
            Expr::Binary { op, lhs, rhs } => self.binary_expr(*op, lhs, rhs),
            Expr::Unary { op, expr } => self.unary_expr(*op, expr),
            Expr::Assign { target, rhs } => self.assign_expr(target, rhs),
            Expr::AssignOp { op, target, rhs } => self.assign_op_expr(*op, target, rhs),
            Expr::Index { target, index } => {
                let addr = self.addr_of_index(target, index);
                self.load(addr)
            }
            Expr::Call { func, args } => self.call_expr(func, args),
            Expr::If {
                test,
                if_block,
                else_ifs,
                else_block,
            } => self.if_expr(test, if_block, else_ifs, else_block.as_ref()),
            Expr::For {
                init,
                test,
                update,
                block,
            } => self.for_expr(init.as_ref(), test.as_deref(), update.as_deref(), block),
            Expr::Return(expr) => {
                let zero = self.const_(0);
                if let Some(expr) = expr {
                    let value = self.expr(expr);
                    self.return_(value);
                    zero
                } else {
                    self.return_(zero);
                    zero
                }
            }
        }
    }

    fn addr_of_expr(&mut self, expr: &AddrOfExpr) -> ValueId {
        match expr {
            AddrOfExpr::Static(id) => self.static_addr(*id),
            AddrOfExpr::Stack(id) => self.stack_addr(*id),
            AddrOfExpr::Index { target, index } => {
                let target = self.expr(target);
                let index = self.expr(index);
                let stride = self.const_(SIZEOF_INT);
                let offset = self.binary(index, stride, BinaryOp::Mul);
                self.binary(target, offset, BinaryOp::Add)
            }
        }
    }

    fn binary_expr(&mut self, op: ast::BinaryOp, lhs: &Expr, rhs: &Expr) -> ValueId {
        macro_rules! binary {
            ($op:ident) => {{
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);
                self.binary(lhs, rhs, BinaryOp::$op)
            }};
        }

        macro_rules! cmp {
            ($op:ident) => {{
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);
                self.cmp(lhs, rhs, CmpOp::$op)
            }};
        }

        match op {
            ast::BinaryOp::Mul => binary!(Mul),
            ast::BinaryOp::Div => binary!(Div),
            ast::BinaryOp::Mod => binary!(Mod),
            ast::BinaryOp::Add => binary!(Add),
            ast::BinaryOp::Sub => binary!(Sub),
            ast::BinaryOp::Shl => binary!(Shl),
            ast::BinaryOp::ArithmeticShr => binary!(ArithmeticShr),
            ast::BinaryOp::LogicalShr => binary!(LogicalShr),
            ast::BinaryOp::Lt => cmp!(Lt),
            ast::BinaryOp::Le => cmp!(Le),
            ast::BinaryOp::Gt => cmp!(Gt),
            ast::BinaryOp::Ge => cmp!(Ge),
            ast::BinaryOp::Eq => cmp!(Eq),
            ast::BinaryOp::Ne => cmp!(Ne),
            ast::BinaryOp::BitAnd => binary!(BitAnd),
            ast::BinaryOp::BitXor => binary!(BitXor),
            ast::BinaryOp::BitOr => binary!(BitOr),
            ast::BinaryOp::LogicalAnd => self.logical_op(lhs, rhs, true),
            ast::BinaryOp::LogicalOr => self.logical_op(lhs, rhs, false),
        }
    }

    fn logical_op(&mut self, lhs: &Expr, rhs: &Expr, is_and: bool) -> ValueId {
        let lhs = self.expr(lhs);
        let rhs_block = self.create_block();
        let final_block = self.create_block();

        if is_and {
            let zero = self.const_(0);
            self.branch(lhs, rhs_block, Vec::new(), final_block, vec![zero]);
        } else {
            let test = self.unary(lhs, UnaryOp::Test);
            self.branch(lhs, final_block, vec![test], rhs_block, Vec::new());
        }

        self.switch_to_block(rhs_block);
        self.seal_block(rhs_block);
        let rhs = self.expr(rhs);
        let test = self.unary(rhs, UnaryOp::Test);
        self.jump(final_block, vec![test]);

        self.switch_to_block(final_block);
        self.seal_block(final_block);
        self.new_block_param(final_block)
    }

    fn unary_expr(&mut self, op: ast::UnaryOp, expr: &Expr) -> ValueId {
        let expr = self.expr(expr);
        match op {
            ast::UnaryOp::Negate => self.unary(expr, UnaryOp::Neg),
            ast::UnaryOp::BitNot => self.unary(expr, UnaryOp::BitNot),
            ast::UnaryOp::LogicalNot => self.unary(expr, UnaryOp::LogicalNot),
            ast::UnaryOp::Deref => self.load(expr),
        }
    }

    fn assign_expr(&mut self, target: &AssignTargetExpr, rhs: &Expr) -> ValueId {
        let rhs = self.expr(rhs);
        match target {
            AssignTargetExpr::Static(id) => self.static_store(*id, rhs),
            AssignTargetExpr::Stack(id) => self.stack_store(*id, rhs),
            AssignTargetExpr::Transient(id) => self.def_variable(*id, rhs),
            AssignTargetExpr::Deref(expr) => {
                let addr = self.expr(expr);
                self.store(addr, rhs);
            }
            AssignTargetExpr::Index { target, index } => {
                let addr = self.addr_of_index(target, index);
                self.store(addr, rhs);
            }
        }
        rhs
    }

    fn addr_of_index(&mut self, target: &Expr, index: &Expr) -> ValueId {
        let base = self.expr(target);
        let index = self.expr(index);
        let size = self.const_(SIZEOF_INT);
        let offset = self.binary(index, size, BinaryOp::Mul);
        self.binary(base, offset, BinaryOp::Add)
    }

    fn assign_op_expr(
        &mut self,
        op: ast::AssignOp,
        target: &AssignTargetExpr,
        rhs: &Expr,
    ) -> ValueId {
        let rhs = self.expr(rhs);

        macro_rules! do_op {
            ($old_val:expr) => {
                match op {
                    ast::AssignOp::Mul => self.binary($old_val, rhs, BinaryOp::Mul),
                    ast::AssignOp::Div => self.binary($old_val, rhs, BinaryOp::Div),
                    ast::AssignOp::Mod => self.binary($old_val, rhs, BinaryOp::Mod),
                    ast::AssignOp::Add => self.binary($old_val, rhs, BinaryOp::Add),
                    ast::AssignOp::Sub => self.binary($old_val, rhs, BinaryOp::Sub),
                    ast::AssignOp::Shl => self.binary($old_val, rhs, BinaryOp::Shl),
                    ast::AssignOp::ArithmeticShr => {
                        self.binary($old_val, rhs, BinaryOp::ArithmeticShr)
                    }
                    ast::AssignOp::LogicalShr => self.binary($old_val, rhs, BinaryOp::LogicalShr),
                    ast::AssignOp::BitAnd => self.binary($old_val, rhs, BinaryOp::BitAnd),
                    ast::AssignOp::BitXor => self.binary($old_val, rhs, BinaryOp::BitXor),
                    ast::AssignOp::BitOr => self.binary($old_val, rhs, BinaryOp::BitOr),
                }
            };
        }

        match target {
            AssignTargetExpr::Static(static_) => {
                let old_val = self.static_load(*static_);
                let new_val = do_op!(old_val);
                self.static_store(*static_, new_val);
                new_val
            }
            AssignTargetExpr::Stack(slot) => {
                let old_val = self.stack_load(*slot);
                let new_val = do_op!(old_val);
                self.stack_store(*slot, new_val);
                new_val
            }
            AssignTargetExpr::Transient(variable) => {
                let old_val = self.use_variable(*variable);
                let new_val = do_op!(old_val);
                self.def_variable(*variable, new_val);
                new_val
            }
            AssignTargetExpr::Deref(addr) => {
                let addr = self.expr(addr);
                let old_val = self.load(addr);
                let new_val = do_op!(old_val);
                self.store(addr, new_val);
                new_val
            }
            AssignTargetExpr::Index { target, index } => {
                let addr = self.addr_of_index(target, index);
                let old_val = self.load(addr);
                let new_val = do_op!(old_val);
                self.store(addr, new_val);
                new_val
            }
        }
    }

    fn call_expr(&mut self, func: &Expr, args: &[Expr]) -> ValueId {
        let func = self.expr(func);
        let args: Vec<_> = args.iter().map(|arg| self.expr(arg)).collect();
        self.call(func, args)
    }

    fn if_expr(
        &mut self,
        test: &Expr,
        if_block: &Block,
        else_ifs: &[ElseIf],
        else_block: Option<&Block>,
    ) -> ValueId {
        let test = self.expr(test);
        let if_block_id = self.create_block();
        let mut next_block = self.create_block();
        let final_block = self.create_block();
        self.branch(test, if_block_id, Vec::new(), next_block, Vec::new());

        self.switch_to_block(if_block_id);
        self.seal_block(if_block_id);
        let value = self.block(if_block);
        self.jump(final_block, vec![value]);

        for else_if in else_ifs {
            self.switch_to_block(next_block);
            self.seal_block(next_block);
            let else_if_block = self.create_block();
            let next_next_block = self.create_block();

            let test = self.expr(&else_if.test);
            self.branch(test, else_if_block, Vec::new(), next_next_block, Vec::new());
            self.switch_to_block(else_if_block);
            self.seal_block(else_if_block);
            let value = self.block(&else_if.block);
            self.jump(final_block, vec![value]);

            next_block = next_next_block;
        }

        self.switch_to_block(next_block);
        self.seal_block(next_block);
        let value = if let Some(block) = else_block {
            self.block(block)
        } else {
            self.const_(0)
        };
        self.jump(final_block, vec![value]);

        self.switch_to_block(final_block);
        self.seal_block(final_block);
        self.new_block_param(final_block)
    }

    fn for_expr(
        &mut self,
        init: Option<&ForInit>,
        test: Option<&Expr>,
        update: Option<&Expr>,
        block: &Block,
    ) -> ValueId {
        match init {
            None => {}
            Some(ForInit::Let { id, expr }) => {
                let expr = self.expr(expr);
                match id {
                    Local::Stack(slot) => self.stack_store(*slot, expr),
                    Local::Transient(variable) => self.def_variable(*variable, expr),
                }
            }
            Some(ForInit::Expr(expr)) => {
                self.expr(expr);
            }
        }

        let body_block = self.create_block();
        let final_block = self.create_block();

        if let Some(test) = test {
            let test = self.expr(test);
            self.branch(test, body_block, Vec::new(), final_block, Vec::new());
        } else {
            self.jump(body_block, Vec::new());
        }

        self.switch_to_block(body_block);
        self.block(block);
        if let Some(update) = update {
            self.expr(update);
        }

        if let Some(test) = test {
            let test = self.expr(test);
            self.branch(test, body_block, Vec::new(), final_block, Vec::new());
        } else {
            self.jump(body_block, Vec::new());
        }

        self.seal_block(body_block);
        self.seal_block(final_block);
        self.switch_to_block(final_block);
        self.const_(0)
    }

    fn next_value_id(&mut self) -> ValueId {
        let inc = self.value_counter + 1;
        std::mem::replace(&mut self.value_counter, inc)
    }

    fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = block;
    }

    fn create_block(&mut self) -> BlockId {
        let id = self.blocks.len();
        self.blocks.push(BasicBlockBuilder {
            predecessors: Vec::new(),
            params: Vec::new(),
            instrs: Vec::new(),
            terminator: None,
            variables: HashMap::new(),
            incomplete: HashMap::new(),
            is_sealed: false,
        });
        id
    }

    fn new_block_param(&mut self, block: BlockId) -> ValueId {
        let param = self.next_value_id();
        self.blocks[block].params.push(param);
        param
    }

    fn seal_block(&mut self, block: BlockId) {
        assert!(!self.blocks[block].is_sealed);
        let preds = self.blocks[block].predecessors.clone();
        for (variable, value) in std::mem::take(&mut self.blocks[block].incomplete) {
            self.blocks[block].params.push(value);
            let mut pred_values = Vec::new();
            for pred in &preds {
                pred_values.push(self.read_variable(variable, pred.block));
            }
            for (value, pred) in pred_values.iter().zip(&preds) {
                let terminator = self.blocks[pred.block].terminator.as_mut().unwrap();
                match pred.kind {
                    PredecessorKind::Jump => terminator.push_jump_value(*value),
                    PredecessorKind::BranchIf => terminator.push_branch_if_value(*value),
                    PredecessorKind::BranchElse => terminator.push_branch_else_value(*value),
                }
            }
        }
        self.blocks[block].is_sealed = true;
    }

    fn def_variable(&mut self, variable: TransientId, value: ValueId) {
        assert!(self.blocks[self.current_block].terminator.is_none());
        self.blocks[self.current_block]
            .variables
            .insert(variable, value);
    }

    fn use_variable(&mut self, variable: TransientId) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        self.read_variable(variable, self.current_block)
    }

    fn read_variable(&mut self, variable: TransientId, block: BlockId) -> ValueId {
        if let Some(value) = self.blocks[block].variables.get(&variable) {
            *value
        } else {
            self.read_variable_recursive(variable, block)
        }
    }

    fn read_variable_recursive(&mut self, variable: TransientId, block: BlockId) -> ValueId {
        if !self.blocks[block].is_sealed {
            let value = self.next_value_id();
            self.blocks[block].incomplete.insert(variable, value);
            self.blocks[block].variables.insert(variable, value);
            value
        } else if self.blocks[block].predecessors.len() == 1 {
            let value = self.read_variable(variable, self.blocks[block].predecessors[0].block);
            self.blocks[block].variables.insert(variable, value);
            value
        } else {
            let value = self.next_value_id();
            self.blocks[block].params.push(value);
            self.blocks[block].variables.insert(variable, value);

            let preds = self.blocks[block].predecessors.clone();
            for pred in preds {
                let pred_value = self.read_variable(variable, pred.block);
                let terminator = self.blocks[pred.block].terminator.as_mut().unwrap();
                match pred.kind {
                    PredecessorKind::Jump => terminator.push_jump_value(pred_value),
                    PredecessorKind::BranchIf => terminator.push_branch_if_value(pred_value),
                    PredecessorKind::BranchElse => terminator.push_branch_if_value(pred_value),
                }
            }
            value
        }
    }

    fn call(&mut self, function: ValueId, args: Vec<ValueId>) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block].instrs.push(Instr::Call {
            target,
            function,
            args,
        });
        target
    }

    fn stack_load(&mut self, slot: StackId) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::StackLoad { target, slot });
        target
    }

    fn stack_store(&mut self, slot: StackId, value: ValueId) {
        assert!(self.blocks[self.current_block].terminator.is_none());
        self.blocks[self.current_block]
            .instrs
            .push(Instr::StackStore { slot, value });
    }

    fn stack_addr(&mut self, slot: StackId) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::StackAddr { target, slot });
        target
    }

    fn static_load(&mut self, static_: StaticId) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::StaticLoad { target, static_ });
        target
    }

    fn static_store(&mut self, static_: StaticId, value: ValueId) {
        assert!(self.blocks[self.current_block].terminator.is_none());
        self.blocks[self.current_block]
            .instrs
            .push(Instr::StaticStore { static_, value });
    }

    fn static_addr(&mut self, static_: StaticId) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::StaticAddr { target, static_ });
        target
    }

    fn load(&mut self, addr: ValueId) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::Load { target, addr });
        target
    }

    fn store(&mut self, addr: ValueId, value: ValueId) {
        assert!(self.blocks[self.current_block].terminator.is_none());
        self.blocks[self.current_block]
            .instrs
            .push(Instr::Store { addr, value });
    }

    fn string(&mut self, string: StringId) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::String { target, string });
        target
    }

    fn function(&mut self, function: FunctionId) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::Function { target, function });
        target
    }

    fn const_(&mut self, value: i64) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::Const { target, value });
        target
    }

    fn unary(&mut self, value: ValueId, op: UnaryOp) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block]
            .instrs
            .push(Instr::Unary { target, op, value });
        target
    }

    fn binary(&mut self, lhs: ValueId, rhs: ValueId, op: BinaryOp) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block].instrs.push(Instr::Binary {
            target,
            op,
            lhs,
            rhs,
        });
        target
    }

    fn cmp(&mut self, lhs: ValueId, rhs: ValueId, op: CmpOp) -> ValueId {
        assert!(self.blocks[self.current_block].terminator.is_none());
        let target = self.next_value_id();
        self.blocks[self.current_block].instrs.push(Instr::Cmp {
            target,
            op,
            lhs,
            rhs,
        });
        target
    }

    fn return_(&mut self, value: ValueId) {
        assert!(self.blocks[self.current_block].terminator.is_none());
        self.blocks[self.current_block].terminator = Some(Terminator::Return(value));
    }

    fn jump(&mut self, block: BlockId, args: Vec<ValueId>) {
        assert!(self.blocks[self.current_block].terminator.is_none());
        self.blocks[self.current_block].terminator = Some(Terminator::Jump { block, args });
        self.blocks[block].predecessors.push(Predecessor {
            block: self.current_block,
            kind: PredecessorKind::Jump,
        });
    }

    fn branch(
        &mut self,
        test: ValueId,
        if_block: BlockId,
        if_args: Vec<ValueId>,
        else_block: BlockId,
        else_args: Vec<ValueId>,
    ) {
        assert!(self.blocks[self.current_block].terminator.is_none());
        self.blocks[self.current_block].terminator = Some(Terminator::Branch {
            test,
            if_block,
            if_args,
            else_block,
            else_args,
        });
        self.blocks[if_block].predecessors.push(Predecessor {
            block: self.current_block,
            kind: PredecessorKind::BranchIf,
        });
        self.blocks[else_block].predecessors.push(Predecessor {
            block: self.current_block,
            kind: PredecessorKind::BranchElse,
        });
    }
}

struct BasicBlockBuilder {
    predecessors: Vec<Predecessor>,
    params: Vec<ValueId>,
    instrs: Vec<Instr>,
    terminator: Option<Terminator>,
    variables: HashMap<TransientId, ValueId>,
    incomplete: HashMap<TransientId, ValueId>,
    is_sealed: bool,
}

impl BasicBlockBuilder {
    fn finish(self) -> BasicBlock {
        assert!(self.is_sealed);
        BasicBlock {
            params: self.params,
            instrs: self.instrs,
            terminator: self.terminator.unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
struct Predecessor {
    block: BlockId,
    kind: PredecessorKind,
}

#[derive(Debug, Clone, Copy)]
enum PredecessorKind {
    Jump,
    BranchIf,
    BranchElse,
}
