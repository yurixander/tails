; ModuleID = 'tests.literals'
source_filename = "tests.literals"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1

define private void @tests_literals.tests() {
fn.entry:
  call void @tests_literals.id(i32 0)
  call void @tests_literals.id(i32 1)
  call void @tests_literals.id(i32 -1)
  call void @tests_literals.id(i32 -2147483648)
  call void @tests_literals.id(i32 -2147483647)
  call void @tests_literals.id(i32 0)
  call void @tests_literals.id.1(i32 0)
  call void @tests_literals.id.1(i32 1)
  call void @tests_literals.id.2(i8 1)
  call void @tests_literals.id.3(i16 1)
  call void @tests_literals.id(i32 1)
  call void @tests_literals.id.4(i64 1)
  call void @tests_literals.id.5(ptr @string_literal)
  call void @tests_literals.id.6(i8 97)
  ret void
}

define private void @tests_literals.id(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_literals.id.1(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_literals.id.2(i8 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_literals.id.3(i16 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_literals.id.4(i64 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_literals.id.5(ptr %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_literals.id.6(i8 %parameter.x) {
fn.entry:
  ret void
}
