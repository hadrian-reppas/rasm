pub struct BuiltinFunction {
    pub name: &'static str,
    pub params: usize,
    pub body: &'static str,
}

pub const BUILTIN_FUNCTIONS: &[BuiltinFunction] = &[
    BuiltinFunction {
        name: "alloc",
        params: 1,
        body: ALLOC_BODY,
    },
    BuiltinFunction {
        name: "free",
        params: 1,
        body: FREE_BODY,
    },
    BuiltinFunction {
        name: "putc",
        params: 1,
        body: PUTC_BODY,
    },
    BuiltinFunction {
        name: "putd",
        params: 1,
        body: PUTD_BODY,
    },
    BuiltinFunction {
        name: "puts",
        params: 1,
        body: PUTS_BODY,
    },
    BuiltinFunction {
        name: "stdin",
        params: 0,
        body: STDIN_BODY,
    },
];

const ALLOC_BODY: &str = r"{
  %2 = mul nsw i64 8, %0
  %3 = call ptr @malloc(i64 %2)
  %4 = ptrtoint ptr %3 to i64
  ret i64 %4
}";

const FREE_BODY: &str = r"{
  %2 = inttoptr i64 %0 to ptr
  call void @free(ptr %2)
  ret i64 0
}";

const PUTC_BODY: &str = r"{
  %2 = call i32 (ptr, ...) @printf(ptr @percent_lc, i64 %0)
  %3 = sext i32 %2 to i64
  %4 = load ptr, ptr @__stdoutp, align 8
  %5 = call i32 @fflush(ptr %4)
  ret i64 %3
}";

const PUTD_BODY: &str = r"{
  %2 = call i32 (ptr, ...) @printf(ptr @percent_ld, i64 %0)
  %3 = sext i32 %2 to i64
  %4 = load ptr, ptr @__stdoutp, align 8
  %5 = call i32 @fflush(ptr %4)
  ret i64 %3
}";

const PUTS_BODY: &str = r"{
  %2 = inttoptr i64 %0 to ptr
  %3 = call i32 (ptr, ...) @printf(ptr @percent_s, ptr %2)
  %4 = sext i32 %3 to i64
  %5 = load ptr, ptr @__stdoutp, align 8
  %6 = call i32 @fflush(ptr %5)
  ret i64 %4
}";

const STDIN_BODY: &str = r"{
  %1 = alloca ptr, align 8
  %2 = alloca i64, align 8
  store ptr null, ptr %1, align 8
  store i64 0, ptr %2, align 8
  %3 = load ptr, ptr @__stdinp, align 8
  %4 = call i64 @getline(ptr %1, ptr %2, ptr %3)
  %5 = icmp eq i64 %4, -1
  br i1 %5, label %6, label %8

6:
  %7 = load ptr, ptr %1, align 8
  call void @free(ptr %7)
  ret i64 0

8:
  %9 = load ptr, ptr %1, align 8
  %10 = ptrtoint ptr %9 to i64
  ret i64 %10
}";
