; ModuleID = 'unit_if'
source_filename = "unit_if"

define private void @tests_unit_if.tests() {
fn.entry:
  call void @tests_unit_if.unbox(ptr null)
  call void @tests_unit_if.unbox(ptr null)
  call void @tests_unit_if.unbox(ptr null)
  ret void
}

define private void @tests_unit_if.unbox(ptr %parameter.a) {
fn.entry:
  ret void
}
