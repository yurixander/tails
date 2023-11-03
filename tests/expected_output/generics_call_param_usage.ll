; ModuleID = 'tests.generics_call_param_usage'
source_filename = "tests.generics_call_param_usage"

define private void @tests_generics_call_param_usage.tests() {
fn.entry:
  %call = call i1 @tests_generics_call_param_usage.foo(i1 true)
  %call1 = call i32 @tests_generics_call_param_usage.foo.1(i32 1)
  ret void
}

define private i1 @tests_generics_call_param_usage.foo(i1 %parameter.a) {
fn.entry:
  ret i1 %parameter.a
}

define private i32 @tests_generics_call_param_usage.foo.1(i32 %parameter.a) {
fn.entry:
  ret i32 %parameter.a
}
