; ModuleID = 'unit_parameter_single'
source_filename = "unit_parameter_single"

define private void @tests_unit_parameter_single.tests() {
fn.entry:
  call void @tests_unit_parameter_single.foo(ptr null)
  ret void
}

define private void @tests_unit_parameter_single.foo(ptr %parameter.x) {
fn.entry:
  ret void
}
