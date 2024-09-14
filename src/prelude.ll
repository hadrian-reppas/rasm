@__stdoutp = external global ptr, align 8
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

define i64 @std.intrinsics.alloc(i64 %0) {
  %2 = mul i64 8, %0
  %3 = call ptr @malloc(i64 %2)
  %4 = ptrtoint ptr %3 to i64
  ret i64 %4
}

define i64 @std.intrinsics.free(i64 %0) {
  %2 = inttoptr i64 %0 to ptr
  call void @free(ptr %2)
  ret i64 0
}

define i64 @std.intrinsics.putc(i64 %0) {
  %2 = call i32 (ptr, ...) @printf(ptr @percent_lc, i64 %0)
  %3 = sext i32 %2 to i64
  %4 = load ptr, ptr @__stdoutp, align 8
  %5 = call i32 @fflush(ptr %4)
  ret i64 %3
}

define i64 @std.intrinsics.putd(i64 %0) {
  %2 = call i32 (ptr, ...) @printf(ptr @percent_ld, i64 %0)
  %3 = sext i32 %2 to i64
  %4 = load ptr, ptr @__stdoutp, align 8
  %5 = call i32 @fflush(ptr %4)
  ret i64 %3
}

define i64 @std.intrinsics.puts(i64 %0) {
  %2 = inttoptr i64 %0 to ptr
  %3 = call i32 (ptr, ...) @printf(ptr @percent_s, ptr %2)
  %4 = sext i32 %3 to i64
  %5 = load ptr, ptr @__stdoutp, align 8
  %6 = call i32 @fflush(ptr %5)
  ret i64 %4
}

define i64 @std.intrinsics.stdin() {
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
}
