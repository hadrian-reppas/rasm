use std::fmt::Write as _;
use std::io::Write as _;

use tempfile::NamedTempFile;

use crate::ast::{AssignOp, BinaryOp, UnaryOp};
use crate::builtins::BUILTIN_FUNCTIONS;
use crate::error::Error;
use crate::resolve::{FunctionId, Local, Resolved, StackId, StaticId, StringId, TransientId};
use crate::resolved::{AddrOfExpr, AssignTargetExpr, Block, Expr, Function, Static};

const RASM_PREFIX: &str = "$";
const PRELUDE: &str = r#"
@__stdoutp= external global ptr, align 8
@__stdinp = external global ptr, align 8

declare i32 @printf(ptr, ...)
declare i64 @getline(ptr, ptr, ptr)
declare ptr @malloc(i64)
declare void @free(ptr)
declare ptr @setlocale(i32, ptr)
declare i32 @fflush(ptr)

@percent_lc = constant [4 x i8] c"%lc\00", align 1
@percent_ld = constant [4 x i8] c"%ld\00", align 1
@percent_s = constant [3 x i8] c"%s\00", align 1
@empty_str = constant [1 x i8] c"\00", align 1

"#;

pub fn generate(resolved: &Resolved, init_order: &[StaticId]) -> Result<NamedTempFile, Error> {
    let mut codegen = Codegen::new(
        &resolved.functions,
        &resolved.statics,
        &resolved.strings,
        init_order,
    );

    codegen.generate();

    let mut file = NamedTempFile::with_prefix(".ll")
        .map_err(|_| Error::msg("cannot create temporary llvm file"))?;
    file.write(codegen.llvm.as_bytes())
        .map_err(|_| Error::msg("cannot write to temporary llvm file"))?;
    Ok(file)
}

type ValueId = usize;

struct Codegen<'a> {
    llvm: String,
    functions: &'a [Function],
    statics: &'a [Static],
    strings: &'a [String],
    init_order: &'a [StaticId],
    id_counter: usize,
    current_block: usize,
}

macro_rules! push {
    ($self:expr, $pat:expr, $($args:expr),*) => {
        write!($self.llvm, $pat, $($args),*).unwrap()
    };

    ($self:expr, $pat:expr) => {
        write!($self.llvm, $pat).unwrap()
    };
}

macro_rules! pushln {
    ($self:expr, $pat:expr, $($args:expr),*) => {
        writeln!($self.llvm, $pat, $($args),*).unwrap()
    };

    ($self:expr, $pat:expr) => {
        writeln!($self.llvm, $pat).unwrap()
    };

    ($self:expr) => {
        $self.llvm.push('\n')
    }
}

impl<'a> Codegen<'a> {
    fn new(
        functions: &'a [Function],
        statics: &'a [Static],
        strings: &'a [String],
        init_order: &'a [StaticId],
    ) -> Self {
        let mut codegen = Codegen {
            llvm: PRELUDE.to_string(),
            functions,
            statics,
            strings,
            init_order,
            id_counter: 1,
            current_block: 0,
        };

        for builtin in BUILTIN_FUNCTIONS {
            push!(codegen, "define i64 @{RASM_PREFIX}{}(", builtin.name);
            for param in 0..builtin.params {
                if param == 0 {
                    push!(codegen, "i64 %0");
                } else {
                    push!(codegen, ", i64 %{param}");
                }
            }
            pushln!(codegen, ") {}\n", builtin.body);
        }

        codegen
    }

    fn next_id(&mut self) -> usize {
        let id = self.id_counter;
        self.id_counter += 1;
        id
    }

    fn enter_block(&mut self, id: usize) {
        pushln!(self, "b{id}:");
        self.current_block = id;
    }

    fn generate(&mut self) {
        for static_ in self.statics {
            pushln!(
                self,
                "@{RASM_PREFIX}{} = internal global i64 0, align 8",
                static_.name
            );
        }
        pushln!(self);

        for (i, string) in self.strings.iter().enumerate() {
            push!(self, "@str{i} = constant [{} x i8] c\"", string.len() + 1);
            self.escaped_chars(string);
            pushln!(self, "\\00\", align 1");
        }
        pushln!(self);

        for function in self.functions {
            self.function(function);
        }

        self.main();
    }

    fn main(&mut self) {
        pushln!(self, "define i32 @main(i32 %0, ptr %1) {{");

        let stack_locals = self
            .statics
            .iter()
            .map(|s| s.stack_locals)
            .max()
            .unwrap_or(0);
        let transient_locals = self
            .statics
            .iter()
            .map(|s| s.transient_locals)
            .max()
            .unwrap_or(0);

        for stack_id in 0..stack_locals {
            pushln!(self, "  %s{stack_id} = alloca i64, align 8");
        }
        for transient_id in 0..transient_locals {
            pushln!(self, "  %t{transient_id} = alloca i64, align 8");
        }
        pushln!(self, "  %x = call ptr @setlocale(i32 0, ptr @empty_str)");

        for static_id in self.init_order {
            let static_ = &self.statics[*static_id];
            let value = self.expr(&static_.expr);
            pushln!(
                self,
                "  store i64 %v{value}, ptr @{RASM_PREFIX}{}, align 8",
                static_.name
            );
        }

        let main = self.functions.iter().find(|f| f.name == "main").unwrap();

        if main.params.is_empty() {
            pushln!(self, "  %mr = call i64 @{RASM_PREFIX}main()");
        } else {
            pushln!(self, "  %argc = sext i32 %0 to i64");
            pushln!(self, "  %argv = ptrtoint ptr %1 to i64");
            pushln!(
                self,
                "  %mr = call i64 @{RASM_PREFIX}main(i64 %argc, i64 %argv)"
            );
        }

        pushln!(self, "  %ec = trunc i64 %mr to i32");
        pushln!(self, "  ret i32 %ec");
        pushln!(self, "}}");
    }

    fn function(&mut self, function: &Function) {
        push!(self, "define i64 @{RASM_PREFIX}{}(", function.name);
        for param in 0..function.params.len() {
            if param == 0 {
                push!(self, "i64 %p0");
            } else {
                push!(self, ", i64 %p{param}");
            }
        }
        pushln!(self, ") {{");
        let start_block = self.next_id();
        pushln!(self, "b{start_block}:");
        self.current_block = start_block;

        for stack_id in 0..function.stack_locals {
            pushln!(self, "  %s{stack_id} = alloca i64, align 8");
        }
        for transient_id in 0..function.transient_locals {
            pushln!(self, "  %t{transient_id} = alloca i64, align 8");
        }

        for (param_id, param) in function.params.iter().enumerate() {
            match param {
                Local::Stack(stack_id) => {
                    pushln!(self, "  store i64 %p{param_id}, ptr %s{stack_id}, align 8");
                }
                Local::Transient(transient_id) => pushln!(
                    self,
                    "  store i64 %p{param_id}, ptr %t{transient_id}, align 8"
                ),
            }
        }

        let value = self.block(&function.block);
        pushln!(self, "  ret i64 %v{value}");

        pushln!(self, "}}\n");
    }

    fn block(&mut self, block: &Block) -> ValueId {
        for stmt in &block.stmts {
            self.expr(stmt);
        }

        if let Some(expr) = &block.expr {
            self.expr(expr)
        } else {
            self.int_literal(0)
        }
    }

    fn expr(&mut self, expr: &Expr) -> ValueId {
        match expr {
            Expr::String(string_id) => self.string_literal(*string_id),
            Expr::Int(value) => self.int_literal(*value),
            Expr::Static(static_id) => self.load_static(*static_id),
            Expr::Function(function_id) => self.function_ref(*function_id),
            Expr::Stack(stack_id) => self.load_stack_variable(*stack_id),
            Expr::Transient(transient_id) => self.load_transient_variable(*transient_id),
            Expr::Block(block) => self.block(block),
            Expr::AddrOf(expr) => self.addr_of_expr(expr),
            Expr::Binary { op, lhs, rhs } => self.binary_expr(*op, lhs, rhs),
            Expr::Unary { op, expr } => self.unary_expr(*op, expr),
            Expr::Assign { target, rhs } => self.assign_expr(target, rhs),
            Expr::AssignOp { op, target, rhs } => self.assign_op_expr(*op, target, rhs),
            Expr::Index { target, index } => self.index_expr(target, index),
            Expr::Call { func, args } => self.call_expr(func, args),
            Expr::If {
                test,
                if_block,
                else_block,
            } => self.if_expr(test, if_block, else_block.as_ref()),
            Expr::For {
                init,
                test,
                update,
                block,
            } => self.for_expr(init.as_deref(), test.as_deref(), update.as_deref(), block),
            Expr::Return(expr) => {
                if let Some(expr) = expr {
                    let value = self.expr(expr);
                    pushln!(self, "  ret i64 %v{value}");
                }
                self.int_literal(0)
            }
        }
    }

    fn string_literal(&mut self, string_id: StringId) -> ValueId {
        let id = self.next_id();
        pushln!(self, "  %v{id} = ptrtoint ptr @str{string_id} to i64");
        id
    }

    fn int_literal(&mut self, value: i64) -> ValueId {
        let id = self.next_id();
        pushln!(self, "  %v{id} = add i64 {value}, 0");
        id
    }

    fn load_static(&mut self, static_id: StaticId) -> ValueId {
        let id = self.next_id();
        pushln!(
            self,
            "  %v{id} = load i64, ptr @{RASM_PREFIX}{}, align 8",
            self.statics[static_id].name
        );
        id
    }

    fn store_static(&mut self, static_id: StaticId, value: ValueId) {
        pushln!(
            self,
            "  store i64 %v{value}, ptr @{RASM_PREFIX}{}, align 8",
            self.statics[static_id].name
        );
    }

    fn function_name(&self, function_id: FunctionId) -> &'a str {
        if let Some(builtin) = BUILTIN_FUNCTIONS.get(function_id) {
            builtin.name
        } else {
            &self.functions[function_id - BUILTIN_FUNCTIONS.len()].name
        }
    }

    fn function_ref(&mut self, function_id: FunctionId) -> ValueId {
        let id = self.next_id();
        pushln!(
            self,
            "  %v{id} = ptrtoint ptr @{RASM_PREFIX}{} to i64",
            self.function_name(function_id)
        );
        id
    }

    fn load_stack_variable(&mut self, stack_id: StackId) -> ValueId {
        let id = self.next_id();
        pushln!(self, "  %v{id} = load i64, ptr %s{stack_id}, align 8");
        id
    }

    fn load_transient_variable(&mut self, transient_id: TransientId) -> ValueId {
        let id = self.next_id();
        pushln!(self, "  %v{id} = load i64, ptr %t{transient_id}, align 8");
        id
    }

    fn store_stack_variable(&mut self, stack_id: StaticId, value: ValueId) {
        pushln!(self, "  store i64 %v{value}, ptr %s{stack_id}, align 8");
    }

    fn store_transient_variable(&mut self, transient_id: TransientId, value: ValueId) {
        pushln!(self, "  store i64 %v{value}, ptr %t{transient_id}, align 8");
    }

    fn addr_of_expr(&mut self, expr: &AddrOfExpr) -> ValueId {
        match expr {
            AddrOfExpr::Static(static_id) => {
                let id = self.next_id();
                pushln!(
                    self,
                    "  %v{id} = ptrtoint ptr @{RASM_PREFIX}{} to i64",
                    self.statics[*static_id].name
                );
                id
            }
            AddrOfExpr::Stack(stack_id) => {
                let id = self.next_id();
                pushln!(self, "  %v{id} = ptrtoint ptr %s{stack_id} to i64");
                id
            }
            AddrOfExpr::Index { target, index } => {
                let addr_ptr = self.index_addr(target, index);
                self.ptr_to_int(addr_ptr)
            }
        }
    }

    fn binary_expr(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> ValueId {
        match op {
            BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod
            | BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Shl
            | BinaryOp::ArithmeticShr
            | BinaryOp::LogicalShr
            | BinaryOp::BitAnd
            | BinaryOp::BitXor
            | BinaryOp::BitOr => self.simple_binary_expr(op, lhs, rhs),
            BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Gt
            | BinaryOp::Ge
            | BinaryOp::Eq
            | BinaryOp::Ne => self.cmp_epxr(op, lhs, rhs),
            BinaryOp::LogicalAnd => self.logical_and_expr(lhs, rhs),
            BinaryOp::LogicalOr => self.logical_or_expr(lhs, rhs),
        }
    }

    fn simple_binary_expr(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> ValueId {
        let instr = match op {
            BinaryOp::Mul => "mul",
            BinaryOp::Div => "sdiv",
            BinaryOp::Mod => "srem",
            BinaryOp::Add => "add",
            BinaryOp::Sub => "sub",
            BinaryOp::Shl => "shl",
            BinaryOp::ArithmeticShr => "ashr",
            BinaryOp::LogicalShr => "lshr",
            BinaryOp::BitAnd => "and",
            BinaryOp::BitXor => "or",
            BinaryOp::BitOr => "xor",
            _ => unreachable!(),
        };

        let lhs_value = self.expr(lhs);
        let rhs_value = self.expr(rhs);
        let id = self.next_id();
        pushln!(self, "  %v{id} = {instr} i64 %v{lhs_value}, %v{rhs_value}");
        id
    }

    fn cmp_epxr(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> ValueId {
        let instr = match op {
            BinaryOp::Lt => "slt",
            BinaryOp::Le => "sle",
            BinaryOp::Gt => "sgt",
            BinaryOp::Ge => "sge",
            BinaryOp::Eq => "eq",
            BinaryOp::Ne => "ne",
            _ => unreachable!(),
        };

        let lhs_value = self.expr(lhs);
        let rhs_value = self.expr(rhs);
        let cmp_value = self.next_id();
        pushln!(
            self,
            "  %v{cmp_value} = icmp {instr} i64 %v{lhs_value}, %v{rhs_value}"
        );

        let id = self.next_id();
        pushln!(self, "  %v{id} = zext i1 %v{cmp_value} to i64");
        id
    }

    fn logical_and_expr(&mut self, lhs: &Expr, rhs: &Expr) -> ValueId {
        let rhs_label = self.next_id();
        let final_label = self.next_id();

        let lhs_value = self.expr(lhs);
        let final_lhs_label = self.current_block;
        let lhs_i1_value = self.next_id();
        pushln!(self, "  %v{lhs_i1_value} = icmp ne i64 %v{lhs_value}, 0");
        pushln!(
            self,
            "  br i1 %v{lhs_i1_value}, label %b{rhs_label}, label %b{final_label}"
        );

        self.enter_block(rhs_label);
        let rhs_value = self.expr(rhs);
        let final_rhs_label = self.current_block;
        let rhs_i1_value = self.next_id();
        pushln!(self, "  %v{rhs_i1_value} = icmp ne i64 %v{rhs_value}, 0");
        let i1_zext_value = self.next_id();
        pushln!(
            self,
            "  %v{i1_zext_value} = zext i1 %v{rhs_i1_value} to i64"
        );
        pushln!(self, "  br label %b{final_label}");

        self.enter_block(final_label);
        let value = self.next_id();
        pushln!(
            self,
            "  %v{value} = phi i64 [ 0, %b{final_lhs_label} ], [ %v{i1_zext_value}, %b{final_rhs_label} ]"
        );
        value
    }

    fn logical_or_expr(&mut self, lhs: &Expr, rhs: &Expr) -> ValueId {
        let rhs_label = self.next_id();
        let final_label = self.next_id();

        let lhs_value = self.expr(lhs);
        let final_lhs_label = self.current_block;
        let lhs_i1_value = self.next_id();
        pushln!(self, "  %v{lhs_i1_value} = icmp ne i64 %v{lhs_value}, 0");
        pushln!(
            self,
            "  br i1 %v{lhs_i1_value}, label %b{final_label}, label %b{rhs_label}"
        );

        self.enter_block(rhs_label);
        let rhs_value = self.expr(rhs);
        let final_rhs_label = self.current_block;
        let rhs_i1_value = self.next_id();
        pushln!(self, "  %v{rhs_i1_value} = icmp ne i64 %v{rhs_value}, 0");
        let i1_zext_value = self.next_id();
        pushln!(
            self,
            "  %v{i1_zext_value} = zext i1 %v{rhs_i1_value} to i64"
        );
        pushln!(self, "  br label %b{final_label}");

        self.enter_block(final_label);
        let value = self.next_id();
        pushln!(
            self,
            "  %v{value} = phi i64 [ 1, %b{final_lhs_label} ], [ %v{i1_zext_value}, %b{final_rhs_label} ]"
        );
        value
    }

    fn unary_expr(&mut self, op: UnaryOp, expr: &Expr) -> ValueId {
        let value = self.expr(expr);
        let id = self.next_id();
        match op {
            UnaryOp::Negate => pushln!(self, "  %v{id} = sub i64 0, %v{value}"),
            UnaryOp::BitNot => pushln!(self, "  %v{id} = xor i64 -1, %v{value}"),
            UnaryOp::LogicalNot => {
                let cmp_value = self.next_id();
                pushln!(self, "  %v{cmp_value} = icmp eq i64 %v{value}, 0");
                pushln!(self, "  %v{id} = zext i1 %v{cmp_value} to i64");
            }
            UnaryOp::Deref => {
                let ptr_value = self.int_to_ptr(value);
                pushln!(self, "  %v{id} = load i64, ptr %v{ptr_value}, align 8");
            }
        }
        id
    }

    fn assign_expr(&mut self, target: &AssignTargetExpr, rhs: &Expr) -> ValueId {
        let value = self.expr(rhs);
        match target {
            AssignTargetExpr::Static(static_id) => self.store_static(*static_id, value),
            AssignTargetExpr::Stack(stack_id) => self.store_stack_variable(*stack_id, value),
            AssignTargetExpr::Transient(transient_id) => {
                self.store_transient_variable(*transient_id, value);
            }
            AssignTargetExpr::Deref(expr) => {
                let addr_value = self.expr(expr);
                let addr_ptr = self.int_to_ptr(addr_value);
                pushln!(self, "  store i64 %v{value}, ptr %v{addr_ptr}, align 8");
            }
            AssignTargetExpr::Index { target, index } => {
                let addr_ptr = self.index_addr(target, index);
                pushln!(self, "  store i64 %v{value}, ptr %v{addr_ptr}, align 8");
            }
        }
        value
    }

    fn assign_op_expr(&mut self, op: AssignOp, target: &AssignTargetExpr, rhs: &Expr) -> ValueId {
        macro_rules! do_op {
            ($old:expr, $rhs:expr) => {{
                let instr = match op {
                    AssignOp::Mul => "mul",
                    AssignOp::Div => "sdiv",
                    AssignOp::Mod => "srem",
                    AssignOp::Add => "add",
                    AssignOp::Sub => "sub",
                    AssignOp::Shl => "shl",
                    AssignOp::ArithmeticShr => "ashr",
                    AssignOp::LogicalShr => "lshr",
                    AssignOp::BitAnd => "and",
                    AssignOp::BitXor => "xor",
                    AssignOp::BitOr => "or",
                };

                let value = self.next_id();
                pushln!(self, "  %v{value} = {instr} i64 %v{}, %v{}", $old, $rhs);
                value
            }};
        }

        let rhs_value = self.expr(rhs);

        match target {
            AssignTargetExpr::Static(static_id) => {
                let old_value = self.load_static(*static_id);
                let value = do_op!(old_value, rhs_value);
                self.store_static(*static_id, value);
                value
            }
            AssignTargetExpr::Stack(stack_id) => {
                let old_value = self.load_stack_variable(*stack_id);
                let value = do_op!(old_value, rhs_value);
                self.store_stack_variable(*stack_id, value);
                value
            }
            AssignTargetExpr::Transient(transient_id) => {
                let old_value = self.load_transient_variable(*transient_id);
                let value = do_op!(old_value, rhs_value);
                self.store_transient_variable(*transient_id, value);
                value
            }
            AssignTargetExpr::Deref(expr) => {
                let addr_value = self.expr(expr);
                let addr_ptr = self.int_to_ptr(addr_value);
                let old_value = self.next_id();
                pushln!(
                    self,
                    "  %v{old_value} = load i64, ptr %v{addr_ptr}, align 8"
                );
                let value = do_op!(old_value, rhs_value);
                pushln!(self, "  store i64 %v{value}, ptr %v{addr_ptr}, align 8");
                value
            }
            AssignTargetExpr::Index { target, index } => {
                let addr_ptr = self.index_addr(target, index);
                let old_value = self.next_id();
                pushln!(
                    self,
                    "  %v{old_value} = load i64, ptr %v{addr_ptr}, align 8"
                );
                let value = do_op!(old_value, rhs_value);
                pushln!(self, "  store i64 %v{value}, ptr %v{addr_ptr}, align 8");
                value
            }
        }
    }

    fn index_addr(&mut self, target: &Expr, index: &Expr) -> ValueId {
        let base_value = self.expr(target);
        let base_ptr = self.int_to_ptr(base_value);
        let index_value = self.expr(index);
        let addr_ptr = self.next_id();
        pushln!(
            self,
            "  %v{addr_ptr} = getelementptr i64, ptr %v{base_ptr}, i64 %v{index_value}"
        );
        addr_ptr
    }

    fn index_expr(&mut self, target: &Expr, index: &Expr) -> ValueId {
        let id = self.next_id();
        let addr_ptr = self.index_addr(target, index);
        pushln!(self, "  %v{id} = load i64, ptr %v{addr_ptr}, align 8");
        id
    }

    fn call_expr(&mut self, func: &Expr, args: &[Expr]) -> ValueId {
        let func_value = self.expr(func);
        let func_ptr = self.int_to_ptr(func_value);

        let arg_values: Vec<_> = args.iter().map(|arg| self.expr(arg)).collect();

        let id = self.next_id();
        push!(self, "  %v{id} = call i64 %v{func_ptr}(");
        for (i, arg_value) in arg_values.iter().enumerate() {
            if i > 0 {
                push!(self, ", ");
            }
            push!(self, "i64 %v{arg_value}");
        }
        pushln!(self, ")");
        id
    }

    fn if_expr(&mut self, test: &Expr, if_block: &Block, else_block: Option<&Block>) -> ValueId {
        let if_label = self.next_id();
        let else_label = self.next_id();
        let final_label = self.next_id();

        let test_value = self.expr(test);
        let i1_value = self.next_id();
        pushln!(self, "  %v{i1_value} = icmp ne i64 %v{test_value}, 0");
        pushln!(
            self,
            "  br i1 %v{i1_value}, label %b{if_label}, label %b{else_label}"
        );

        self.enter_block(if_label);
        let if_value = self.block(if_block);
        pushln!(self, "  br label %b{final_label}");
        let final_if_label = self.current_block;

        self.enter_block(else_label);
        let else_value = if let Some(else_block) = else_block {
            self.block(else_block)
        } else {
            self.int_literal(0)
        };
        pushln!(self, "  br label %b{final_label}");
        let final_else_label = self.current_block;

        self.enter_block(final_label);
        let id = self.next_id();
        pushln!(
            self,
            "  %v{id} = phi i64 [ %v{if_value}, %b{final_if_label} ], [ %v{else_value}, %b{final_else_label} ]"
        );
        id
    }

    fn for_expr(
        &mut self,
        init: Option<&Expr>,
        test: Option<&Expr>,
        update: Option<&Expr>,
        block: &Block,
    ) -> ValueId {
        if let Some(init) = init {
            self.expr(init);
        }

        let for_body = self.next_id();
        let final_label = self.next_id();

        if let Some(test) = test {
            let test_value = self.expr(test);
            let i1_value = self.next_id();
            pushln!(self, "  %v{i1_value} = icmp ne i64 %v{test_value}, 0");
            pushln!(
                self,
                "  br i1 %v{i1_value}, label %b{for_body}, label %b{final_label}"
            );
        } else {
            pushln!(self, "  br label %b{for_body}");
        }

        self.enter_block(for_body);
        self.block(block);

        if let Some(update) = update {
            self.expr(update);
        }

        if let Some(test) = test {
            let test_value = self.expr(test);
            let i1_value = self.next_id();
            pushln!(self, "  %v{i1_value} = icmp ne i64 %v{test_value}, 0");
            pushln!(
                self,
                "  br i1 %v{i1_value}, label %b{for_body}, label %b{final_label}"
            );
        } else {
            pushln!(self, "  br label %b{for_body}");
        }

        self.enter_block(final_label);
        self.int_literal(0)
    }

    fn int_to_ptr(&mut self, value: ValueId) -> ValueId {
        let id = self.next_id();
        pushln!(self, "  %v{id} = inttoptr i64 %v{value} to ptr");
        id
    }

    fn ptr_to_int(&mut self, value: ValueId) -> ValueId {
        let id = self.next_id();
        pushln!(self, "  %v{id} = ptrtoint ptr %v{value} to i64");
        id
    }

    fn escaped_chars(&mut self, str: &str) {
        for &byte in str.as_bytes() {
            if matches!(byte, b' ' | b'!' | b'#'..=b'[' | b']'..=b'~') {
                self.llvm.push(byte.try_into().unwrap());
            } else {
                push!(self, "\\{:02x}", byte);
            }
        }
    }
}
