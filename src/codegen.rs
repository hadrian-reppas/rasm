use std::collections::{BTreeSet, HashMap};
use std::io::Write;
use std::process::Command;

use codegen::ir::{FuncRef, GlobalValue, SigRef, StackSlot};
use cranelift::prelude::*;
use cranelift_codegen::{isa, settings, Context};
use cranelift_frontend::FunctionBuilderContext;
use cranelift_module::{default_libcall_names, DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use tempfile::NamedTempFile;

use crate::ast::{AssignOp, BinaryOp, UnaryOp};
use crate::builtins::{BuiltinFunction, BUILTIN_FUNCTIONS};
use crate::error::{Error, Span};
use crate::resolve::{FunctionId, Local, Resolved, StaticId, StringId};
use crate::resolved::{
    AddrOfExpr, AssignTargetExpr, Block, ElseIf, Expr, ForInit, Function, Static, Stmt,
};

const SIZEOF_INT: usize = 8;

pub fn compile(
    resolved: &Resolved,
    init_order: &[StaticId],
    output_file_path: &str,
) -> Result<(), Error> {
    let mut codegen = Codegen::new(resolved);

    let init_id = *codegen.function_ids.last().unwrap();
    for (function, id) in resolved.functions.iter().zip(
        codegen
            .function_ids
            .clone()
            .into_iter()
            .skip(BUILTIN_FUNCTIONS.len()),
    ) {
        if function.name == "main" && !init_order.is_empty() {
            codegen.compile_function(function, id, Some(init_id));
        } else {
            codegen.compile_function(function, id, None);
        }
    }

    if !init_order.is_empty() {
        let init = make_init_function(&resolved.statics, init_order);
        codegen.compile_function(&init, init_id, None);
    }

    let builtins: BTreeSet<_> = resolved
        .functions
        .iter()
        .flat_map(|f| &f.function_dependencies)
        .chain(
            resolved
                .statics
                .iter()
                .flat_map(|s| &s.function_dependencies),
        )
        .copied()
        .filter(|id| *id < BUILTIN_FUNCTIONS.len())
        .collect();
    for id in builtins {
        codegen.compile_builtin(id, BUILTIN_FUNCTIONS[id]);
    }

    let object = codegen.module.finish().object;
    let bytes = object.write().unwrap();
    let mut file = NamedTempFile::new().map_err(|_| Error {
        msg: "cannot create tempfile".to_string(),
        span: Span::empty(),
    })?;
    file.write(&bytes).map_err(|_| Error {
        msg: "cannot write to tempfile".to_string(),
        span: Span::empty(),
    })?;

    // TODO: REMOVE
    std::fs::File::create("temp.o")
        .unwrap()
        .write(&bytes)
        .unwrap();

    let result = Command::new("cc")
        .arg("-o")
        .arg(output_file_path)
        .arg("-lc")
        .arg(file.path())
        .status();

    match result {
        Ok(status) if status.success() => Ok(()),
        _ => Err(Error {
            msg: "linker error".to_string(),
            span: Span::empty(),
        }),
    }
}

fn make_init_function(statics: &[Static], init_order: &[StaticId]) -> Function {
    let mut stmts = Vec::new();
    for static_id in init_order {
        stmts.push(Stmt::Expr(Expr::Assign {
            target: AssignTargetExpr::Static(*static_id),
            rhs: Box::new(statics[*static_id].expr.clone()),
        }));
    }
    let block = Block { stmts, expr: None };

    let transient_locals = statics.iter().map(|s| s.transient_locals).max().unwrap();
    let stack_locals = statics.iter().map(|s| s.stack_locals).max().unwrap();

    Function {
        name: String::new(),
        span: Span::empty(),
        id: FunctionId::MAX,
        params: Vec::new(),
        block,
        transient_locals,
        stack_locals,
        static_dependencies: Vec::new(),
        function_dependencies: Vec::new(),
    }
}

pub struct Codegen {
    pub module: ObjectModule,
    pub context: Context,
    pub builder_context: FunctionBuilderContext,
    pub string_ids: Vec<DataId>,
    pub static_ids: Vec<DataId>,
    pub function_ids: Vec<FuncId>,
    pub int: Type,
}

impl Codegen {
    pub fn new(resolved: &Resolved) -> Self {
        let shared_flags = settings::Flags::new(settings::builder());
        let triple = target_lexicon::triple!("x86_64-apple-darwin-macho");
        let isa_builder = isa::lookup(triple).unwrap();
        let isa = isa_builder.finish(shared_flags).unwrap();
        let builder =
            ObjectBuilder::new(isa, "builder".to_string(), default_libcall_names()).unwrap();
        let mut module = ObjectModule::new(builder);
        let mut context = module.make_context();
        let builder_context = FunctionBuilderContext::new();
        let int = Type::int(8 * (SIZEOF_INT as u16)).unwrap();

        let mut description = DataDescription::new();
        let mut string_ids = Vec::new();
        for (i, string) in resolved.strings.iter().enumerate() {
            let null_terminated: Vec<_> = string.bytes().chain([0]).collect();
            let id = module
                .declare_data(&format!("${i}"), Linkage::Local, false, false)
                .unwrap();
            description.define(null_terminated.into());
            description.align = Some(SIZEOF_INT as u64);
            module.define_data(id, &description).unwrap();
            description.clear();
            string_ids.push(id);
        }

        let mut static_ids = Vec::new();
        for static_ in &resolved.statics {
            let id = module
                .declare_data(&format!("_{}", static_.name), Linkage::Local, true, false)
                .unwrap();
            description.define_zeroinit(SIZEOF_INT);
            description.align = Some(SIZEOF_INT as u64);
            module.define_data(id, &description).unwrap();
            description.clear();
            static_ids.push(id);
        }

        let mut function_ids = Vec::new();
        for builtin in BUILTIN_FUNCTIONS {
            for _ in 0..builtin.params {
                context.func.signature.params.push(AbiParam::new(int));
            }
            context.func.signature.returns.push(AbiParam::new(int));
            let id = module
                .declare_function(
                    &format!("_{}", builtin.name),
                    Linkage::Export,
                    &context.func.signature,
                )
                .unwrap();
            function_ids.push(id);
            context.clear();
        }
        for function in &resolved.functions {
            for _ in &function.params {
                context.func.signature.params.push(AbiParam::new(int));
            }
            context.func.signature.returns.push(AbiParam::new(int));
            let under_name = format!("_{}", function.name);
            let id = module
                .declare_function(
                    if function.name == "main" {
                        "main"
                    } else {
                        &under_name
                    },
                    Linkage::Export,
                    &context.func.signature,
                )
                .unwrap();
            function_ids.push(id);
            context.clear();
        }

        if !resolved.statics.is_empty() {
            let id = module
                .declare_function("init", Linkage::Export, &context.func.signature)
                .unwrap();
            function_ids.push(id);
        }

        Codegen {
            module,
            context,
            builder_context,
            string_ids,
            static_ids,
            function_ids,
            int,
        }
    }

    fn compile_function(&mut self, function: &Function, id: FuncId, init_id: Option<FuncId>) {
        let int = AbiParam::new(self.int);
        for _ in &function.params {
            self.context.func.signature.params.push(int);
        }
        self.context.func.signature.returns.push(int);

        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);

        let mut stack_slots = Vec::new();
        for _ in 0..function.stack_locals {
            let slot = builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                SIZEOF_INT as u32,
            ));
            stack_slots.push(slot);
        }

        let mut variables = Vec::new();
        for id in 0..function.transient_locals {
            let var = Variable::from_u32(id as u32);
            builder.declare_var(var, self.int);
            variables.push(var);
        }

        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        builder.seal_block(block);
        if let Some(init_id) = init_id {
            let func_ref = self.module.declare_func_in_func(init_id, builder.func);
            builder.ins().call(func_ref, &[]);
        }

        for (i, param) in function.params.iter().enumerate() {
            let val = builder.block_params(block)[i];
            match param {
                Local::Stack(id) => {
                    builder.ins().stack_load(self.int, stack_slots[*id], 0);
                }
                Local::Transient(id) => builder.def_var(variables[*id], val),
            }
        }

        let mut translator = Translator::new(
            builder,
            stack_slots,
            variables,
            &mut self.module,
            &self.string_ids,
            &self.static_ids,
            &self.function_ids,
            self.int,
        );
        let ret = translator.block(&function.block);
        translator.builder.ins().return_(&[ret]);
        translator.builder.finalize();

        self.module.define_function(id, &mut self.context).unwrap();
        self.module.clear_context(&mut self.context);
    }

    fn compile_builtin(&mut self, id: FunctionId, builtin: BuiltinFunction) {
        let int = AbiParam::new(self.int);
        for _ in 0..builtin.params {
            self.context.func.signature.params.push(int);
        }
        self.context.func.signature.returns.push(int);

        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);

        (builtin.generate)(&mut self.module, &mut builder, self.int);

        self.module
            .define_function(self.function_ids[id], &mut self.context)
            .unwrap();
        self.module.clear_context(&mut self.context);
    }
}

struct Translator<'a> {
    builder: FunctionBuilder<'a>,
    stack_slots: Vec<StackSlot>,
    variables: Vec<Variable>,
    module: &'a mut ObjectModule,
    string_ids: &'a [DataId],
    static_ids: &'a [DataId],
    function_ids: &'a [FuncId],
    string_globals: Vec<Option<GlobalValue>>,
    static_globals: Vec<Option<GlobalValue>>,
    function_refs: Vec<Option<FuncRef>>,
    sigrefs: HashMap<usize, SigRef>,
    int: Type,
}

impl<'a> Translator<'a> {
    fn new(
        builder: FunctionBuilder<'a>,
        stack_slots: Vec<StackSlot>,
        variables: Vec<Variable>,
        module: &'a mut ObjectModule,
        string_ids: &'a [DataId],
        static_ids: &'a [DataId],
        function_ids: &'a [FuncId],
        int: Type,
    ) -> Self {
        Translator {
            builder,
            stack_slots,
            variables,
            module,
            string_ids,
            static_ids,
            function_ids,
            string_globals: vec![None; string_ids.len()],
            static_globals: vec![None; static_ids.len()],
            function_refs: vec![None; function_ids.len()],
            sigrefs: HashMap::new(),
            int,
        }
    }

    fn block(&mut self, block: &Block) -> Value {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Let { id, expr } => {
                    let val = self.expr(expr);
                    match id {
                        Local::Stack(id) => {
                            let slot = self.stack_slots[*id];
                            self.builder.ins().stack_store(val, slot, 0);
                        }
                        Local::Transient(id) => self.builder.def_var(self.variables[*id], val),
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
            self.builder.ins().iconst(self.int, 0)
        }
    }

    fn expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::String(id) => self.get_string(*id),
            Expr::Int(i) => self.builder.ins().iconst(self.int, *i),
            Expr::Static(id) => self.get_static(*id),
            Expr::Function(id) => self.get_function(*id),
            Expr::Stack(id) => self
                .builder
                .ins()
                .stack_load(self.int, self.stack_slots[*id], 0),
            Expr::Transient(id) => self.builder.use_var(self.variables[*id]),
            Expr::Block(block) => self.block(block),
            Expr::AddrOf(expr) => self.addr_of(expr),
            Expr::Binary { op, lhs, rhs } => self.binary(*op, lhs, rhs),
            Expr::Unary { op, expr } => self.unary(*op, expr),
            Expr::Assign { target, rhs } => self.assign(target, rhs),
            Expr::AssignOp { op, target, rhs } => self.assign_op(*op, target, rhs),
            Expr::Index { target, index } => self.index(target, index),
            Expr::Call { func, args } => self.call(func, args),
            Expr::If {
                test,
                if_block,
                else_ifs,
                else_block,
            } => self.if_(test, if_block, else_ifs, else_block.as_ref()),
            Expr::For {
                init,
                test,
                update,
                block,
            } => self.for_(init.as_ref(), test.as_deref(), update.as_deref(), block),
            Expr::Return(expr) => self.return_(expr.as_deref()),
        }
    }

    fn addr_of(&mut self, expr: &AddrOfExpr) -> Value {
        match expr {
            AddrOfExpr::Static(id) => self.get_static_ref(*id),
            AddrOfExpr::Stack(id) => {
                self.builder
                    .ins()
                    .stack_addr(self.int, self.stack_slots[*id], 0)
            }
            AddrOfExpr::Index { target, index } => {
                let target = self.expr(target);
                let index = self.expr(index);
                let offset = self.builder.ins().imul_imm(index, SIZEOF_INT as i64);
                self.builder.ins().iadd(target, offset)
            }
        }
    }

    fn binary(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> Value {
        let lhs = self.expr(lhs);

        macro_rules! op {
            ($op:ident $(, $icc:ident)?) => {{
                let rhs = self.expr(rhs);
                self.builder.ins().$op($(IntCC::$icc,)? lhs, rhs)
            }};
        }

        match op {
            BinaryOp::Mul => op!(imul),
            BinaryOp::Div => op!(sdiv),
            BinaryOp::Mod => op!(srem),
            BinaryOp::Add => op!(iadd),
            BinaryOp::Sub => op!(isub),
            BinaryOp::Shl => op!(ishl),
            BinaryOp::ArithmeticShr => op!(sshr),
            BinaryOp::LogicalShr => op!(ushr),
            BinaryOp::Lt => op!(icmp, SignedLessThan),
            BinaryOp::Le => op!(icmp, SignedLessThanOrEqual),
            BinaryOp::Gt => op!(icmp, SignedGreaterThan),
            BinaryOp::Ge => op!(icmp, SignedGreaterThanOrEqual),
            BinaryOp::Eq => op!(icmp, Equal),
            BinaryOp::Ne => op!(icmp, NotEqual),
            BinaryOp::BitAnd => op!(band),
            BinaryOp::BitXor => op!(bxor),
            BinaryOp::BitOr => op!(bor),
            BinaryOp::LogicalAnd => self.logical_op(lhs, rhs, true),
            BinaryOp::LogicalOr => self.logical_op(lhs, rhs, false),
        }
    }

    fn logical_op(&mut self, lhs: Value, rhs: &Expr, is_and: bool) -> Value {
        let rhs_block = self.builder.create_block();
        let final_block = self.builder.create_block();
        self.builder.append_block_param(final_block, self.int);

        if is_and {
            let zero = self.builder.ins().iconst(self.int, 0);
            self.builder
                .ins()
                .brif(lhs, rhs_block, &[], final_block, &[zero]);
        } else {
            let one = self.builder.ins().iconst(self.int, 1);
            self.builder
                .ins()
                .brif(lhs, final_block, &[one], rhs_block, &[]);
        }

        self.builder.switch_to_block(rhs_block);
        self.builder.seal_block(rhs_block);
        let rhs = self.expr(rhs);
        let zero = self.builder.ins().iconst(self.int, 0);
        let one = self.builder.ins().iconst(self.int, 1);
        let value = self.builder.ins().select(rhs, one, zero);
        self.builder.ins().jump(final_block, &[value]);

        self.builder.switch_to_block(final_block);
        self.builder.seal_block(final_block);
        self.builder.block_params(final_block)[0]
    }

    fn unary(&mut self, op: UnaryOp, expr: &Expr) -> Value {
        let expr = self.expr(expr);
        match op {
            UnaryOp::Negate => self.builder.ins().ineg(expr),
            UnaryOp::BitNot => self.builder.ins().bnot(expr),
            UnaryOp::LogicalNot => {
                let zero = self.builder.ins().iconst(self.int, 0);
                let one = self.builder.ins().iconst(self.int, 1);
                self.builder.ins().select(expr, zero, one)
            }
            UnaryOp::Deref => self
                .builder
                .ins()
                .load(self.int, MemFlags::trusted(), expr, 0),
        }
    }

    fn assign(&mut self, target: &AssignTargetExpr, rhs: &Expr) -> Value {
        let rhs = self.expr(rhs);
        match target {
            AssignTargetExpr::Static(id) => self.set_static(*id, rhs),
            AssignTargetExpr::Stack(id) => {
                self.builder
                    .ins()
                    .stack_store(rhs, self.stack_slots[*id], 0);
            }
            AssignTargetExpr::Transient(id) => {
                let variable = self.variables[*id];
                self.builder.def_var(variable, rhs);
            }
            AssignTargetExpr::Deref(expr) => {
                let addr = self.expr(expr);
                self.builder.ins().store(MemFlags::trusted(), rhs, addr, 0);
            }
            AssignTargetExpr::Index { target, index } => {
                let target = self.expr(target);
                let index = self.expr(index);
                let offset = self.builder.ins().imul_imm(index, SIZEOF_INT as i64);
                let addr = self.builder.ins().iadd(target, offset);
                self.builder.ins().store(MemFlags::trusted(), rhs, addr, 0);
            }
        }
        rhs
    }

    fn assign_op(&mut self, op: AssignOp, target: &AssignTargetExpr, rhs: &Expr) -> Value {
        macro_rules! combine {
            ($old_val:expr, $rhs:expr) => {
                match op {
                    AssignOp::Mul => self.builder.ins().imul($old_val, $rhs),
                    AssignOp::Div => self.builder.ins().sdiv($old_val, $rhs),
                    AssignOp::Mod => self.builder.ins().srem($old_val, $rhs),
                    AssignOp::Add => self.builder.ins().iadd($old_val, $rhs),
                    AssignOp::Sub => self.builder.ins().isub($old_val, $rhs),
                    AssignOp::Shl => self.builder.ins().ishl($old_val, $rhs),
                    AssignOp::ArithmeticShr => self.builder.ins().sshr($old_val, $rhs),
                    AssignOp::LogicalShr => self.builder.ins().ushr($old_val, $rhs),
                    AssignOp::BitAnd => self.builder.ins().band($old_val, $rhs),
                    AssignOp::BitXor => self.builder.ins().bxor($old_val, $rhs),
                    AssignOp::BitOr => self.builder.ins().bor($old_val, $rhs),
                }
            };
        }

        let rhs = self.expr(rhs);

        match target {
            AssignTargetExpr::Static(id) => {
                let old_val = self.get_static(*id);
                let new_val = combine!(old_val, rhs);
                self.set_static(*id, new_val);
                new_val
            }
            AssignTargetExpr::Stack(id) => {
                let old_val = self
                    .builder
                    .ins()
                    .stack_load(self.int, self.stack_slots[*id], 0);
                let new_val = combine!(old_val, rhs);
                self.builder
                    .ins()
                    .stack_store(new_val, self.stack_slots[*id], 0);
                new_val
            }
            AssignTargetExpr::Transient(id) => {
                let variable = self.variables[*id];
                let old_val = self.builder.use_var(variable);
                let new_val = combine!(old_val, rhs);
                self.builder.def_var(variable, new_val);
                new_val
            }
            AssignTargetExpr::Deref(expr) => {
                let addr = self.expr(expr);
                let old_val = self
                    .builder
                    .ins()
                    .load(self.int, MemFlags::trusted(), addr, 0);
                let new_val = combine!(old_val, rhs);
                self.builder
                    .ins()
                    .store(MemFlags::trusted(), new_val, addr, 0);
                new_val
            }
            AssignTargetExpr::Index { target, index } => {
                let target = self.expr(target);
                let index = self.expr(index);
                let offset = self.builder.ins().imul_imm(index, SIZEOF_INT as i64);
                let addr = self.builder.ins().iadd(target, offset);
                let old_val = self
                    .builder
                    .ins()
                    .load(self.int, MemFlags::trusted(), addr, 0);
                let new_val = combine!(old_val, rhs);
                self.builder
                    .ins()
                    .store(MemFlags::trusted(), new_val, addr, 0);
                new_val
            }
        }
    }

    fn index(&mut self, target: &Expr, index: &Expr) -> Value {
        let target = self.expr(target);
        let index = self.expr(index);
        let offset = self.builder.ins().imul_imm(index, SIZEOF_INT as i64);
        let addr = self.builder.ins().iadd(target, offset);
        self.builder
            .ins()
            .load(self.int, MemFlags::trusted(), addr, 0)
    }

    fn call(&mut self, func: &Expr, args: &[Expr]) -> Value {
        let sigref = self.get_sigref(args.len());
        let func = self.expr(func);
        let args: Vec<_> = args.iter().map(|arg| self.expr(arg)).collect();
        let inst = self.builder.ins().call_indirect(sigref, func, &args);
        self.builder.inst_results(inst)[0]
    }

    fn if_(
        &mut self,
        test: &Expr,
        if_block: &Block,
        else_ifs: &[ElseIf],
        else_block: Option<&Block>,
    ) -> Value {
        let test = self.expr(test);
        let if_block_label = self.builder.create_block();
        let mut next_block = self.builder.create_block();
        let final_block = self.builder.create_block();
        self.builder.append_block_param(final_block, self.int);
        self.builder
            .ins()
            .brif(test, if_block_label, &[], next_block, &[]);

        self.builder.switch_to_block(if_block_label);
        self.builder.seal_block(if_block_label);
        let value = self.block(if_block);
        self.builder.ins().jump(final_block, &[value]);

        for else_if in else_ifs {
            self.builder.switch_to_block(next_block);
            self.builder.seal_block(next_block);
            let else_if_block = self.builder.create_block();
            let next_next_block = self.builder.create_block();

            let test = self.expr(&else_if.test);
            self.builder
                .ins()
                .brif(test, else_if_block, &[], next_next_block, &[]);
            self.builder.switch_to_block(else_if_block);
            self.builder.seal_block(else_if_block);
            let value = self.block(&else_if.block);
            self.builder.ins().jump(final_block, &[value]);

            next_block = next_next_block;
        }

        self.builder.switch_to_block(next_block);
        self.builder.seal_block(next_block);
        let value = if let Some(block) = else_block {
            self.block(block)
        } else {
            self.builder.ins().iconst(self.int, 0)
        };
        self.builder.ins().jump(final_block, &[value]);

        self.builder.switch_to_block(final_block);
        self.builder.seal_block(final_block);
        self.builder.block_params(final_block)[0]
    }

    fn for_(
        &mut self,
        init: Option<&ForInit>,
        test: Option<&Expr>,
        update: Option<&Expr>,
        block: &Block,
    ) -> Value {
        match init {
            None => {}
            Some(ForInit::Let { id, expr }) => {
                let expr = self.expr(expr);
                match id {
                    Local::Stack(id) => {
                        self.builder
                            .ins()
                            .stack_store(expr, self.stack_slots[*id], 0);
                    }
                    Local::Transient(id) => self.builder.def_var(self.variables[*id], expr),
                }
            }
            Some(ForInit::Expr(expr)) => {
                self.expr(expr);
            }
        }

        let body_block = self.builder.create_block();
        let final_block = self.builder.create_block();

        if let Some(test) = test {
            let test = self.expr(test);
            self.builder
                .ins()
                .brif(test, body_block, &[], final_block, &[]);
        } else {
            self.builder.ins().jump(body_block, &[]);
        }

        self.builder.switch_to_block(body_block);
        self.block(block);
        if let Some(update) = update {
            self.expr(update);
        }

        if let Some(test) = test {
            let test = self.expr(test);
            self.builder
                .ins()
                .brif(test, body_block, &[], final_block, &[]);
        } else {
            self.builder.ins().jump(body_block, &[]);
        }

        self.builder.seal_block(body_block);
        self.builder.seal_block(final_block);
        self.builder.switch_to_block(final_block);
        self.builder.ins().iconst(self.int, 0)
    }

    fn return_(&mut self, expr: Option<&Expr>) -> Value {
        let zero = self.builder.ins().iconst(self.int, 0);
        if let Some(expr) = expr {
            let val = self.expr(expr);
            self.builder.ins().return_(&[val]);
        } else {
            self.builder.ins().return_(&[zero]);
        }
        zero
    }

    fn get_string(&mut self, id: StringId) -> Value {
        if let Some(global) = self.string_globals[id] {
            self.builder.ins().global_value(self.int, global)
        } else {
            let global = self
                .module
                .declare_data_in_func(self.string_ids[id], self.builder.func);
            self.string_globals[id] = Some(global);
            self.builder.ins().global_value(self.int, global)
        }
    }

    fn get_static_global_value(&mut self, id: StaticId) -> GlobalValue {
        if let Some(global) = self.static_globals[id] {
            global
        } else {
            let global = self
                .module
                .declare_data_in_func(self.static_ids[id], self.builder.func);
            self.static_globals[id] = Some(global);
            global
        }
    }

    fn get_static_ref(&mut self, id: StaticId) -> Value {
        let global = self.get_static_global_value(id);
        self.builder.ins().global_value(self.int, global)
    }

    fn set_static(&mut self, id: StaticId, value: Value) {
        let ptr = self.get_static_ref(id);
        self.builder.ins().store(MemFlags::trusted(), value, ptr, 0);
    }

    fn get_static(&mut self, id: StaticId) -> Value {
        let ptr = self.get_static_ref(id);
        self.builder
            .ins()
            .load(self.int, MemFlags::trusted(), ptr, 0)
    }

    fn get_function_ref(&mut self, id: FunctionId) -> FuncRef {
        if let Some(func_ref) = self.function_refs[id] {
            func_ref
        } else {
            let func_ref = self
                .module
                .declare_func_in_func(self.function_ids[id], self.builder.func);
            self.function_refs[id] = Some(func_ref);
            func_ref
        }
    }

    fn get_function(&mut self, id: FunctionId) -> Value {
        let func_ref = self.get_function_ref(id);
        self.builder.ins().func_addr(self.int, func_ref)
    }

    fn get_sigref(&mut self, args: usize) -> SigRef {
        if let Some(sigref) = self.sigrefs.get(&args) {
            *sigref
        } else {
            let mut signature = self.module.make_signature();
            for _ in 0..args {
                signature.params.push(AbiParam::new(self.int));
            }
            signature.returns.push(AbiParam::new(self.int));
            let sigref = self.builder.import_signature(signature);
            self.sigrefs.insert(args, sigref);
            sigref
        }
    }
}
