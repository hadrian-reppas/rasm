use crate::resolve::StringId;

pub const BUILTIN_FUNCTIONS: &[&str] = &["alloc", "free", "puts", "putd", "stdin"];
pub const BUILTIN_STRINGS: &[&str] = &["%d"];
pub const PERCENT_D_STRING_ID: StringId = 0;
