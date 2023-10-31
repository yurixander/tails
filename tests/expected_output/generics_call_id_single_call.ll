; ModuleID = 'generics_call_id_single_call'
source_filename = "generics_call_id_single_call"

define private void @tests_generics_call_id_single_call.tests() {
fn.entry:
  call void @tests_generics_call_id_single_call.id(i32 123)
  ret void
}

define private void @tests_generics_call_id_single_call.id(i32 %parameter.x) {
fn.entry:
  ret void
}
