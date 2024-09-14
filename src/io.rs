use std::collections::HashMap;
use std::path::Path;

use crate::ast::Name;
use crate::error::Error;

lazy_static::lazy_static! {
    static ref STD_MAP: HashMap<&'static [&'static str], &'static str> = HashMap::from([
        (["mod.rasm"].as_slice(), include_str!("../std/mod.rasm")),
        (["string.rasm"].as_slice(), include_str!("../std/string.rasm")),
        (["util.rasm"].as_slice(), include_str!("../std/util.rasm")),
        (["cstr.rasm"].as_slice(), include_str!("../std/cstr.rasm")),
        (["mem.rasm"].as_slice(), include_str!("../std/mem.rasm")),
        (["alloc.rasm"].as_slice(), include_str!("../std/alloc.rasm")),
        (["intrinsics.rasm"].as_slice(), include_str!("../std/intrinsics.rasm")),
    ]);
}

pub fn load_source(path: SourcePath) -> Result<&'static str, Error> {
    match path {
        SourcePath::Std(path) => Ok(load_std_file(path).unwrap()),
        SourcePath::Path(path) => {
            let code = std::fs::read_to_string(path)
                .map_err(|_| Error::msg(format!("cannot read file {path:?}")))?;
            Ok(code.leak())
        }
    }
}

fn load_std_file(path: &[&str]) -> Option<&'static str> {
    STD_MAP.get(path).copied()
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SourcePath {
    Std(&'static [&'static str]),
    Path(&'static Path),
}

impl SourcePath {
    pub fn push(self, name: &Name) -> Result<Self, Error> {
        match self {
            SourcePath::Std(path) => {
                let prefix = &path[..path.len() - 1];
                let mod_file_path = [prefix, &[&name.name, "mod.rasm"]].concat();

                if load_std_file(&mod_file_path).is_some() {
                    let mod_file_path = [prefix, &[name.name.clone().leak(), "mod.rasm"]].concat();
                    Ok(SourcePath::Std(mod_file_path.leak()))
                } else {
                    let file_name = format!("{}.rasm", name.name).leak();
                    let file_path = [prefix, &[file_name]].concat();
                    Ok(SourcePath::Std(file_path.leak()))
                }
            }
            SourcePath::Path(path) => {
                let (mut file_path, mut mod_file_path) = (path.to_path_buf(), path.to_path_buf());
                file_path.pop();
                file_path.push(&format!("{}.rasm", name.name));
                mod_file_path.pop();
                mod_file_path.push(&format!("{}/mod.rasm", name.name));

                if file_path.exists() && mod_file_path.exists() {
                    Err(Error::new(
                        name.span,
                        format!("both {file_path:?} and {mod_file_path:?} exist"),
                    ))
                } else if file_path.exists() {
                    let new_path: &'static Path = Box::leak(Box::new(file_path));
                    Ok(SourcePath::Path(new_path))
                } else if mod_file_path.exists() {
                    let new_path: &'static Path = Box::leak(Box::new(mod_file_path));
                    Ok(SourcePath::Path(new_path))
                } else {
                    Err(Error::msg(format!(
                        "neither {file_path:?} or {mod_file_path:?} exists"
                    )))
                }
            }
        }
    }

    pub fn is_std(self) -> bool {
        matches!(self, SourcePath::Std(_))
    }
}
