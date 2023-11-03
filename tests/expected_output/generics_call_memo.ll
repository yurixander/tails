; ModuleID = 'tests.generics_call_memo'
source_filename = "tests.generics_call_memo"

@string_literal = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@string_literal.2 = private unnamed_addr constant [6 x i8] c"world\00", align 1

define private void @tests_generics_call_memo.tests() {
fn.entry:
  call void @tests_generics_call_memo.id(i32 1)
  call void @tests_generics_call_memo.id(i32 2)
  call void @tests_generics_call_memo.id(i32 3)
  call void @tests_generics_call_memo.id.1(ptr @string_literal)
  call void @tests_generics_call_memo.id.1(ptr @string_literal.2)
  ret void
}

define private void @tests_generics_call_memo.id(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_generics_call_memo.id.1(ptr %parameter.x) {
fn.entry:
  ret void
}
