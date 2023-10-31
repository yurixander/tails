; ModuleID = 'function_return'
source_filename = "function_return"

define private void @tests_function_return.tests() {
fn.entry:
  %call = call i32 @tests_function_return.foo()
  ret void
}

define private i32 @tests_function_return.foo() {
fn.entry:
  ret i32 123
}
