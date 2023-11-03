; ModuleID = 'tests.function_param'
source_filename = "tests.function_param"

define private void @tests_function_param.tests() {
fn.entry:
  call void @tests_function_param.foo(i32 123)
  ret void
}

define private void @tests_function_param.foo(i32 %parameter.x) {
fn.entry:
  ret void
}
