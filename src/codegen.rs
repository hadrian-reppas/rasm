use std::fmt::Write as _;
use std::io::Write as _;

use tempfile::NamedTempFile;

use crate::ast::BinaryOp;
use crate::builtins::BUILTIN_FUNCTIONS;
use crate::error::Error;
use crate::resolve::{FunctionId, Local, Resolved, StaticId, StringId};
use crate::resolved::{AddrOfExpr, Block, Expr, Function, Static, Stmt};

const RASM_PREFIX: &'static str = "$";
const PRELUDE: &'static str = r#"
@__stdinp = external global ptr, align 8

declare i32 @printf(ptr, ...)
declare i64 @getline(ptr, ptr, ptr)
declare ptr @malloc(i64)
declare void @free(ptr)

@percent_ld = constant [4 x i8] c"%ld\00", align 1
@percent_s = constant [3 x i8] c"%s\00", align 1

"#;

pub fn generate(resolved: &Resolved, init_order: &[StaticId]) -> Result<NamedTempFile, Error> {
    let mut codegen = Codegen::new(
        &resolved.functions,
        &resolved.statics,
        &resolved.strings,
        init_order,
    );

    codegen.generate();

    println!("{}", codegen.llvm);

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
    value_id_counter: ValueId,
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
            value_id_counter: 0,
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

    fn value_id(&mut self) -> ValueId {
        let id = self.value_id_counter;
        self.value_id_counter += 1;
        id
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

        pushln!(self, "  ; TODO: initialize statics");

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

        for stack_id in 0..function.stack_locals {
            pushln!(self, "  %s{stack_id} = alloca i64, align 8");
        }
        for transient_id in 0..function.transient_locals {
            pushln!(self, "  %t{transient_id} = alloca i64, align 8");
        }

        for (param_id, param) in function.params.iter().enumerate() {
            match param {
                Local::Stack(stack_id) => {
                    pushln!(self, "  store i64 %p{param_id}, ptr %s{stack_id}, align 8")
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
            self.stmt(stmt);
        }

        if let Some(expr) = &block.expr {
            self.expr(expr)
        } else {
            self.int_literal(0)
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { id, expr } => {
                let value = self.expr(expr);
                match id {
                    Local::Stack(stack_id) => {
                        pushln!(self, "  store i64 %v{value}, ptr %s{stack_id}, align 8")
                    }
                    Local::Transient(transient_id) => {
                        pushln!(self, "  store i64 %v{value}, ptr %t{transient_id}, align 8")
                    }
                }
            }
            Stmt::Expr(expr) => {
                self.expr(expr);
            }
        }
    }

    fn expr(&mut self, expr: &Expr) -> ValueId {
        match expr {
            Expr::String(string_id) => self.string_literal(*string_id),
            Expr::Int(value) => self.int_literal(*value),
            Expr::Static(static_id) => self.load_static(*static_id),
            Expr::Function(function_id) => self.function_ref(*function_id),
            Expr::Stack(stack_id) => self.variable(*stack_id, "s"),
            Expr::Transient(transient_id) => self.variable(*transient_id, "t"),
            Expr::Block(block) => self.block(block),
            Expr::AddrOf(expr) => self.addr_of_expr(expr),
            Expr::Binary { op, lhs, rhs } => self.binary_expr(*op, lhs, rhs),
            Expr::Unary { op, expr } => todo!(),
            Expr::Assign { target, rhs } => todo!(),
            Expr::AssignOp { op, target, rhs } => todo!(),
            Expr::Index { target, index } => self.index_expr(target, index),
            Expr::Call { func, args } => self.call_expr(func, args),
            Expr::If {
                test,
                if_block,
                else_ifs,
                else_block,
            } => todo!(),
            Expr::For {
                init,
                test,
                update,
                block,
            } => todo!(),
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
        let id = self.value_id();
        pushln!(self, "  %v{id} = ptrtoint ptr @str{string_id} to i64");
        id
    }

    fn int_literal(&mut self, value: i64) -> ValueId {
        let id = self.value_id();
        pushln!(self, "  %v{id} = add i64 {value}, 0");
        id
    }

    fn load_static(&mut self, static_id: StaticId) -> ValueId {
        let id = self.value_id();
        pushln!(
            self,
            "  %v{id} = load i64, ptr @{RASM_PREFIX}{}, align 8",
            self.statics[static_id].name
        );
        id
    }

    fn function_name(&self, function_id: FunctionId) -> &'a str {
        if let Some(builtin) = BUILTIN_FUNCTIONS.get(function_id) {
            builtin.name
        } else {
            &self.functions[function_id - 5].name
        }
    }

    fn function_ref(&mut self, function_id: FunctionId) -> ValueId {
        let id = self.value_id();
        pushln!(
            self,
            "  %v{id} = ptrtoint ptr @{RASM_PREFIX}{} to i64",
            self.function_name(function_id)
        );
        id
    }

    fn variable(&mut self, variable_id: usize, prefix: &str) -> ValueId {
        let id = self.value_id();
        pushln!(
            self,
            "  %v{id} = load i64, ptr %{prefix}{variable_id}, align 8"
        );
        id
    }

    fn addr_of_expr(&mut self, expr: &AddrOfExpr) -> ValueId {
        match expr {
            AddrOfExpr::Static(static_id) => {
                let id = self.value_id();
                pushln!(
                    self,
                    "  %v{id} = ptrtoint ptr @{RASM_PREFIX}{} to i64",
                    self.statics[*static_id].name
                );
                id
            }
            AddrOfExpr::Stack(stack_id) => {
                let id = self.value_id();
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
        let id = self.value_id();
        pushln!(self, "  %v{id} = {instr} i64 %v{lhs_value}, %v{rhs_value}");
        id
    }

    fn cmp_epxr(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> ValueId {
        todo!()
    }

    fn logical_and_expr(&mut self, lhs: &Expr, rhs: &Expr) -> ValueId {
        todo!()
    }

    fn logical_or_expr(&mut self, lhs: &Expr, rhs: &Expr) -> ValueId {
        todo!()
    }

    fn index_addr(&mut self, target: &Expr, index: &Expr) -> ValueId {
        let base_value = self.expr(target);
        let base_ptr = self.int_to_ptr(base_value);
        let index_value = self.expr(index);
        let addr_ptr = self.value_id();
        pushln!(
            self,
            "  %v{addr_ptr} = getelementptr i64, ptr %v{base_ptr}, i64 %v{index_value}"
        );
        addr_ptr
    }

    fn index_expr(&mut self, target: &Expr, index: &Expr) -> ValueId {
        let id = self.value_id();
        let addr_ptr = self.index_addr(target, index);
        pushln!(self, "  %v{id} = load i64, ptr %v{addr_ptr}, align 8");
        id
    }

    fn call_expr(&mut self, func: &Expr, args: &[Expr]) -> ValueId {
        let func_value = self.expr(func);
        let func_ptr = self.int_to_ptr(func_value);

        let arg_values: Vec<_> = args.iter().map(|arg| self.expr(arg)).collect();

        let id = self.value_id();
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

    fn int_to_ptr(&mut self, value: ValueId) -> ValueId {
        let id = self.value_id();
        pushln!(self, "  %v{id} = inttoptr i64 %v{value} to ptr");
        id
    }

    fn ptr_to_int(&mut self, value: ValueId) -> ValueId {
        let id = self.value_id();
        pushln!(self, "  %v{id} = ptrtoint ptr %v{value} to i64");
        id
    }

    fn escaped_chars(&mut self, str: &str) {
        for &byte in str.as_bytes() {
            if matches!(byte, b' ' | b'!' | b'#'..=b'[' | b']'..=b'~') {
                self.llvm.push(byte.try_into().unwrap());
            } else {
                push!(self, "\\{:02x}", byte)
            }
        }
    }
}
