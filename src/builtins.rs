use cranelift::prelude::*;
use cranelift_frontend::FunctionBuilder;
use cranelift_module::{Linkage, Module};
use cranelift_object::ObjectModule;

use crate::codegen::Codegen;
use crate::resolve::StringId;

pub const BUILTIN_FUNCTIONS: &[BuiltinFunction] = &[
    BuiltinFunction {
        name: "alloc",
        params: 1,
        generate: generate_alloc,
    },
    BuiltinFunction {
        name: "free",
        params: 1,
        generate: generate_free,
    },
    BuiltinFunction {
        name: "puts",
        params: 1,
        generate: generate_puts,
    },
    BuiltinFunction {
        name: "putd",
        params: 1,
        generate: generate_putd,
    },
    BuiltinFunction {
        name: "stdin",
        params: 0,
        generate: generate_stdin,
    },
];

pub const BUILTIN_STRINGS: &[&str] = &["%d"];
pub const PERCENT_D_STRING_ID: StringId = 0;

#[derive(Clone, Copy)]
pub struct BuiltinFunction {
    pub name: &'static str,
    pub params: usize,
    pub generate: fn(&mut ObjectModule, &mut FunctionBuilder, Type),
}

fn generate_alloc(module: &mut ObjectModule, builder: &mut FunctionBuilder, int: Type) {
    let mut signature = module.make_signature();
    signature.params.push(AbiParam::new(int));
    signature.returns.push(AbiParam::new(int));

    let func_id = module
        .declare_function("malloc", Linkage::Import, &signature)
        .unwrap();
    let func_ref = module.declare_func_in_func(func_id, &mut builder.func);

    let block = builder.create_block();
    builder.append_block_params_for_function_params(block);
    builder.switch_to_block(block);
    builder.seal_block(block);

    let ints = builder.block_params(block)[0];
    let bytes = builder.ins().imul_imm(ints, 8);
    // let inst = builder.ins().call(func_ref, &[bytes]);
    // let ptr = builder.inst_results(inst)[0];
    // builder.ins().return_(&[ptr]);
    builder.ins().return_(&[bytes]);
}

fn generate_free(module: &mut ObjectModule, builder: &mut FunctionBuilder, int: Type) {
    todo!()
}

fn generate_puts(module: &mut ObjectModule, builder: &mut FunctionBuilder, int: Type) {
    todo!()
}

fn generate_putd(module: &mut ObjectModule, builder: &mut FunctionBuilder, int: Type) {
    todo!()
}

fn generate_stdin(module: &mut ObjectModule, builder: &mut FunctionBuilder, int: Type) {
    todo!()
}
