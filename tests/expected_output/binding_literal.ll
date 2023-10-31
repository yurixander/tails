; ModuleID = 'binding_literal'
source_filename = "binding_literal"

define private void @tests_binding_literal.tests() {
fn.entry:
  call void @tests_binding_literal.id(i32 0)
  call void @tests_binding_literal.id(i32 1)
  call void @tests_binding_literal.id(i32 -2147483648)
  call void @tests_binding_literal.id(i32 -2147483647)
  call void @tests_binding_literal.id(i32 0)
  call void @tests_binding_literal.id.1(i32 0)
  call void @tests_binding_literal.id.1(i32 1)
  call void @tests_binding_literal.id.2(i8 1)
  call void @tests_binding_literal.id.3(i16 1)
  call void @tests_binding_literal.id(i32 1)
  call void @tests_binding_literal.id.4(i64 1)
  call void @tests_binding_literal.id.5(float 3.140000e+02)
  call void @tests_binding_literal.id.6(ptr null)
  call void @tests_binding_literal.id.7(i8 97)
  ret void
}

define private void @tests_binding_literal.id(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding_literal.id.1(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding_literal.id.2(i8 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding_literal.id.3(i16 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding_literal.id.4(i64 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding_literal.id.5(float %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding_literal.id.6(ptr %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding_literal.id.7(i8 %parameter.x) {
fn.entry:
  ret void
}
