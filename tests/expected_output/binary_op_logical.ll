; ModuleID = 'binary_op_logical'
source_filename = "binary_op_logical"

define private void @tests_binary_op_logical.tests() {
fn.entry:
  call void @tests_binary_op_logical.id(i1 false)
  call void @tests_binary_op_logical.id(i1 true)
  call void @tests_binary_op_logical.id(i1 false)
  call void @tests_binary_op_logical.id(i1 true)
  call void @tests_binary_op_logical.id(i1 true)
  call void @tests_binary_op_logical.id(i1 true)
  call void @tests_binary_op_logical.id(i1 false)
  call void @tests_binary_op_logical.id(i1 false)
  call void @tests_binary_op_logical.id(i1 true)
  call void @tests_binary_op_logical.id(i1 false)
  call void @tests_binary_op_logical.id(i1 true)
  ret void
}

define private void @tests_binary_op_logical.id(i1 %parameter.x) {
fn.entry:
  ret void
}
