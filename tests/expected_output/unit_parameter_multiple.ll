; ModuleID = 'unit_parameter_multiple'
source_filename = "unit_parameter_multiple"

define private void @tests_unit_parameter_multiple.tests() {
fn.entry:
  call void @tests_unit_parameter_multiple.foo(ptr null, ptr null)
  ret void
}

define private void @tests_unit_parameter_multiple.foo(ptr %parameter.x, ptr %parameter.y) {
fn.entry:
  ret void
}
