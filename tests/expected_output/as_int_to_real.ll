; ModuleID = 'as_int_to_real'
source_filename = "as_int_to_real"

define private void @tests_as_int_to_real.tests() {
fn.entry:
  call void @tests_as_int_to_real.a(half 0xH57B0)
  call void @tests_as_int_to_real.a.1(float 1.230000e+02)
  call void @tests_as_int_to_real.a.2(double 1.230000e+02)
  call void @tests_as_int_to_real.a.3(i32 123)
  call void @tests_as_int_to_real.a.4(i16 123)
  call void @tests_as_int_to_real.a.5(i8 123)
  ret void
}

define private void @tests_as_int_to_real.a(half %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_real.a.1(float %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_real.a.2(double %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_real.a.3(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_real.a.4(i16 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_real.a.5(i8 %parameter.x) {
fn.entry:
  ret void
}
