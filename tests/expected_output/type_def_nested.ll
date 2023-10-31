; ModuleID = 'type_def_nested'
source_filename = "type_def_nested"

define private void @tests_type_def_nested.tests() {
fn.entry:
  call void @tests_type_def_nested.unbox(i32 1)
  call void @tests_type_def_nested.unbox(i32 2)
  call void @tests_type_def_nested.unbox(i32 3)
  ret void
}

define private void @tests_type_def_nested.unbox(i32 %parameter.x) {
fn.entry:
  ret void
}
