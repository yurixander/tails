; ModuleID = 'tests.generics_call_zero'
source_filename = "tests.generics_call_zero"

define private void @tests_generics_call_zero.tests() {
fn.entry:
  call void @tests_generics_call_zero.zero()
  call void @tests_generics_call_zero.zero()
  call void @tests_generics_call_zero.zero()
  call void @tests_generics_call_zero.zero()
  ret void
}

define private void @tests_generics_call_zero.zero() {
fn.entry:
  ret void
}
