; ModuleID = 'foreign_varargs'
source_filename = "foreign_varargs"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1
@string_literal.1 = private unnamed_addr constant [8 x i8] c"test %d\00", align 1
@string_literal.2 = private unnamed_addr constant [11 x i8] c"test %d %d\00", align 1
@string_literal.3 = private unnamed_addr constant [14 x i8] c"test %d %d %d\00", align 1
@string_literal.4 = private unnamed_addr constant [8 x i8] c"test %f\00", align 1

define private void @tests_foreign_varargs.tests() {
fn.entry:
  %call = call i32 (ptr, ...) @printf(ptr @string_literal)
  %call1 = call i32 (ptr, i32, ...) @printf(ptr @string_literal.1, i32 1)
  %call2 = call i32 (ptr, i32, i32, ...) @printf(ptr @string_literal.2, i32 1, i32 2)
  %call3 = call i32 (ptr, i32, i32, i32, ...) @printf(ptr @string_literal.3, i32 1, i32 2, i32 3)
  %call4 = call i32 (ptr, float, ...) @printf(ptr @string_literal.4, float 3.140000e+02)
  ret void
}

declare i32 @printf(ptr, ...)
