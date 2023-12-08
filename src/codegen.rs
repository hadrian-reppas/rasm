use crate::error::Error;
use crate::resolve::{Resolved, StaticId};
use crate::resolved::*;

pub fn compile(
    resolved: &Resolved,
    init_order: &[StaticId],
    output_file_path: &str,
) -> Result<(), Error> {
    todo!()
}

pub struct Codegen {}

impl Codegen {
    pub fn new() -> Self {
        todo!()
    }

    pub fn compile(&mut self, items: &[Item]) {
        todo!()
    }
}
