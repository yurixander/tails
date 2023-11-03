; ModuleID = 'tests.unary_op'
source_filename = "tests.unary_op"

define private void @tests_unary_op.tests() {
fn.entry:
  call void @tests_unary_op.id(i1 false)
  call void @tests_unary_op.id(i1 true)
  call void @tests_unary_op.id(i1 true)
  call void @tests_unary_op.id.1(i32 -1)
  call void @tests_unary_op.id.1(i32 -123)
  call void @tests_unary_op.id.1(i32 123)
  ret void
}

define private void @tests_unary_op.id(i1 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_unary_op.id.1(i32 %parameter.x) {
fn.entry:
  ret void
}
