; ModuleID = 'tests.generics_full_hints'
source_filename = "tests.generics_full_hints"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1

define private void @tests_generics_full_hints.tests() {
fn.entry:
  %call = call i32 @tests_generics_full_hints.id(i32 123)
  %call1 = call float @tests_generics_full_hints.id.1(float 3.140000e+02)
  %call2 = call ptr @tests_generics_full_hints.id.2(ptr null)
  %call3 = call ptr @tests_generics_full_hints.id.3(ptr @string_literal)
  ret void
}

define private i32 @tests_generics_full_hints.id(i32 %parameter.x) {
fn.entry:
  ret i32 %parameter.x
}

define private float @tests_generics_full_hints.id.1(float %parameter.x) {
fn.entry:
  ret float %parameter.x
}

define private ptr @tests_generics_full_hints.id.2(ptr %parameter.x) {
fn.entry:
  ret ptr %parameter.x
}

define private ptr @tests_generics_full_hints.id.3(ptr %parameter.x) {
fn.entry:
  ret ptr %parameter.x
}
