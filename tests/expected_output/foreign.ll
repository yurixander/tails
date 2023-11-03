; ModuleID = 'tests.foreign'
source_filename = "tests.foreign"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1
@string_literal.1 = private unnamed_addr constant [9 x i8] c"hello %s\00", align 1
@string_literal.2 = private unnamed_addr constant [6 x i8] c"world\00", align 1
@stdin = external global ptr
@stdout = external global ptr

define private void @tests_foreign.tests() {
fn.entry:
  %call = call i32 @puts(ptr @string_literal)
  %call1 = call i32 (ptr, ptr, ...) @printf(ptr @string_literal.1, ptr @string_literal.2)
  %call2 = call i32 @getline(ptr null, ptr null, ptr @stdin)
  %call3 = call ptr @fgets(ptr null, i32 5, ptr @stdin)
  call void @free(ptr @stdout)
  %call4 = call i64 @strtol(ptr @string_literal, ptr null, i32 10)
  ret void
}

declare i32 @puts(ptr)

declare i32 @printf(ptr, ...)

declare i32 @getline(ptr, ptr, ptr)

declare ptr @fgets(ptr, i32, ptr)

declare void @free(ptr)

declare i64 @strtol(ptr, ptr, i32)
