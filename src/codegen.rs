use std::io::Write;

use tempfile::NamedTempFile;

use crate::builtins::BUILTIN_FUNCTIONS;
use crate::error::Error;
use crate::resolve::{Resolved, StaticId};
use crate::resolved::{Function, Static};

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

struct Codegen<'a> {
    llvm: String,
    functions: &'a [Function],
    statics: &'a [Static],
    strings: &'a [String],
    init_order: &'a [StaticId],
    counter: usize,
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
            counter: 0,
        };

        for builtin in BUILTIN_FUNCTIONS {
            codegen.push(&format!("define i64 @{RASM_PREFIX}{}(", builtin.name));
            for param in 0..builtin.params {
                if param == 0 {
                    codegen.push(&format!("i64 %0"));
                } else {
                    codegen.push(&format!(", i64 %{param}"));
                }
            }
            codegen.push(&format!(") {}\n\n", builtin.body));
        }

        codegen
    }

    fn push(&mut self, s: &str) {
        self.llvm.push_str(s);
    }

    fn generate(&mut self) {
        for static_ in self.statics {
            self.push(&format!(
                "@{RASM_PREFIX}{} = internal global i64 0, align 8\n",
                static_.name
            ));
        }
        self.push("\n");

        for (i, string) in self.strings.iter().enumerate() {
            self.push(&format!(
                "@str{i} = constant [{} x i8] c\"",
                string.len() + 1
            ));
            self.escaped_chars(string);
            self.push("\\00\", align 1\n");
        }
        self.push("\n");

        for function in self.functions {
            self.function(function);
        }

        self.main();
    }

    fn main(&mut self) {
        self.push("define i32 @main(i32 %0, ptr %1) {\n");

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

        for stack_local in 0..stack_locals {
            self.push(&format!("  %s{stack_local} = alloca i64, align 8\n"));
        }
        for transient_local in 0..transient_locals {
            self.push(&format!("  %t{transient_local} = alloca i64, align 8\n"));
        }

        self.push("  ; TODO: initialize statics\n");

        let main = self.functions.iter().find(|f| f.name == "main").unwrap();

        if main.params.is_empty() {
            self.push("  %mr = call i64 @{RASM_PREFIX}main()\n");
        } else {
            self.push("  %argc = sext i32 %0 to i64\n");
            self.push("  %argv = ptrtoint ptr %1 to i64\n");
            self.push(&format!(
                "  %mr = call i64 @{RASM_PREFIX}main(i64 %argc, i64 %argv)\n"
            ));
        }

        self.push("  %ec = trunc i64 %mr to i32\n");
        self.push("  ret i32 %ec\n");
        self.push("}\n");
    }

    fn function(&mut self, function: &Function) {
        self.push(&format!("define i64 @{RASM_PREFIX}{}(", function.name));
        for param in 0..function.params.len() {
            if param == 0 {
                self.push("i64 %a0");
            } else {
                self.push(&format!(", i64 %a{param}"));
            }
        }
        self.push(") {\n");

        self.push("  ; TODO: function body\n");
        self.push("  ret i64 0\n");

        self.push("}\n\n");
    }

    fn escaped_chars(&mut self, str: &str) {
        for &byte in str.as_bytes() {
            if matches!(byte, b' ' | b'!' | b'#'..=b'[' | b']'..=b'~') {
                self.llvm.push(byte.try_into().unwrap());
            } else {
                self.push(&format!("\\{:02x}", byte))
            }
        }
    }
}
