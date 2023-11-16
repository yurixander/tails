; ModuleID = 'tests.infer_binary_op'
source_filename = "tests.infer_binary_op"

define private void @tests_infer_binary_op.tests() {
fn.entry:
  call void @tests_infer_binary_op.id(i32 246)
  call void @tests_infer_binary_op.id.1(i8 2)
  call void @tests_infer_binary_op.id.2(i16 4)
  ret void
}

define private void @tests_infer_binary_op.id(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_infer_binary_op.id.1(i8 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_infer_binary_op.id.2(i16 %parameter.x) {
fn.entry:
  ret void
}
