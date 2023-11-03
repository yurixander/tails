; ModuleID = 'tests.as_int_to_int'
source_filename = "tests.as_int_to_int"

define private void @tests_as_int_to_int.tests() {
fn.entry:
  call void @tests_as_int_to_int.a(i32 123)
  call void @tests_as_int_to_int.a.1(i32 123)
  call void @tests_as_int_to_int.a.1(i32 123)
  call void @tests_as_int_to_int.a(i32 123)
  call void @tests_as_int_to_int.a.2(i16 123)
  call void @tests_as_int_to_int.a(i32 123)
  call void @tests_as_int_to_int.a.3(i64 123)
  call void @tests_as_int_to_int.a(i32 123)
  call void @tests_as_int_to_int.a.2(i16 123)
  call void @tests_as_int_to_int.a.4(i8 123)
  ret void
}

define private void @tests_as_int_to_int.a(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_int.a.1(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_int.a.2(i16 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_int.a.3(i64 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_as_int_to_int.a.4(i8 %parameter.x) {
fn.entry:
  ret void
}
