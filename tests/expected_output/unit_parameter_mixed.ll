; ModuleID = 'unit_parameter_mixed'
source_filename = "unit_parameter_mixed"

define private void @tests_unit_parameter_mixed.tests() {
fn.entry:
  %call = call i32 @tests_unit_parameter_mixed.foo(ptr null, ptr null, i32 1)
  ret void
}

define private i32 @tests_unit_parameter_mixed.foo(ptr %parameter.x, ptr %parameter.y, i32 %parameter.z) {
fn.entry:
  ret i32 %parameter.z
}
