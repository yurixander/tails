; ModuleID = 'function_empty'
source_filename = "function_empty"

define private void @tests_function_empty.tests() {
fn.entry:
  call void @tests_function_empty.foo()
  ret void
}

define private void @tests_function_empty.foo() {
fn.entry:
  ret void
}
