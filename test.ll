@__stdinp = external global ptr, align 8

declare i32 @printf(ptr, ...)
declare i64 @getline(ptr, ptr, ptr)
declare ptr @malloc(i64)
declare void @free(ptr)
declare ptr @setlocale(i32, ptr)

@percent_lc = constant [4 x i8] c"%lc\00", align 1
@percent_ld = constant [4 x i8] c"%ld\00", align 1
@percent_s = constant [3 x i8] c"%s\00", align 1
@empty_str = constant [1 x i8] c"\00", align 1

define i64 @$alloc(i64 %0) {
  %2 = mul nsw i64 8, %0
  %3 = call ptr @malloc(i64 %2)
  %4 = ptrtoint ptr %3 to i64
  ret i64 %4
}

define i64 @$free(i64 %0) {
  %2 = inttoptr i64 %0 to ptr
  call void @free(ptr %2)
  ret i64 0
}

define i64 @$putc(i64 %0) {
  %2 = call i32 (ptr, ...) @printf(ptr @percent_lc, i64 %0)
  %3 = sext i32 %2 to i64
  ret i64 %3
}

define i64 @$putd(i64 %0) {
  %2 = call i32 (ptr, ...) @printf(ptr @percent_ld, i64 %0)
  %3 = sext i32 %2 to i64
  ret i64 %3
}

define i64 @$puts(i64 %0) {
  %2 = inttoptr i64 %0 to ptr
  %3 = call i32 (ptr, ...) @printf(ptr @percent_s, ptr %2)
  %4 = sext i32 %3 to i64
  ret i64 %4
}

define i64 @$stdin() {
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



define i64 @$main(i64 %p0, i64 %p1) {
  %t0 = alloca i64, align 8
  %t1 = alloca i64, align 8
  store i64 %p0, ptr %t0, align 8
  store i64 %p1, ptr %t1, align 8
  %v0 = ptrtoint ptr @$fib to i64
  %v1 = inttoptr i64 %v0 to ptr
  %v2 = load i64, ptr %t0, align 8
  %v3 = call i64 %v1(i64 %v2)
  ret i64 %v3
}

define i64 @$fib(i64 %p0) {
  %t0 = alloca i64, align 8
  store i64 %p0, ptr %t0, align 8
  %v7 = load i64, ptr %t0, align 8
  %v8 = add i64 1, 0
  %v9 = icmp eq i64 %v7, %v8
  %v10 = zext i1 %v9 to i64
  %v11 = icmp ne i64 %v10, 0
  br i1 %v11, label %b4, label %b5

b4:
  %v12 = add i64 1, 0
  br label %b6

b5:
  %v16 = load i64, ptr %t0, align 8
  %v17 = add i64 1, 0
  %v18 = icmp sgt i64 %v16, %v17
  %v19 = zext i1 %v18 to i64
  %v20 = icmp ne i64 %v19, 0
  br i1 %v20, label %b13, label %b14

b13:
  %v21 = ptrtoint ptr @$fib to i64
  %v22 = inttoptr i64 %v21 to ptr
  %v23 = load i64, ptr %t0, align 8
  %v24 = add i64 1, 0
  %v25 = sub i64 %v23, %v24
  %v26 = call i64 %v22(i64 %v25)
  %v27 = ptrtoint ptr @$fib to i64
  %v28 = inttoptr i64 %v27 to ptr
  %v29 = load i64, ptr %t0, align 8
  %v30 = add i64 2, 0
  %v31 = sub i64 %v29, %v30
  %v32 = call i64 %v28(i64 %v31)
  %v33 = add i64 %v26, %v32
  br label %b15

b14:
  %v34 = add i64 0, 0
  br label %b15

b15:
  %v35 = phi i64 [ %v33, %b13 ], [ %v34, %b14 ]
  br label %b6

b6:
  %v36 = phi i64 [ %v12, %b4 ], [ %v35, %b5 ]
  ret i64 %v36
}

define i32 @main(i32 %0, ptr %1) {
  %x = call ptr @setlocale(i32 0, ptr @empty_str)
  %argc = sext i32 %0 to i64
  %argv = ptrtoint ptr %1 to i64
  %mr = call i64 @$main(i64 %argc, i64 %argv)
  %ec = trunc i64 %mr to i32
  ret i32 %ec
}
