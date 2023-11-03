; ModuleID = 'tests.generics_unconstrained'
source_filename = "tests.generics_unconstrained"

define private void @tests_generics_unconstrained.tests() {
fn.entry:
  call void @tests_generics_unconstrained.a(i32 123)
  call void @tests_generics_unconstrained.a.1(i32 123)
  call void @tests_generics_unconstrained.a.2(float 1.200000e+01)
  ret void
}

define private void @tests_generics_unconstrained.a(i32 %parameter.x) {
fn.entry:
  %int.add_op = add i32 %parameter.x, %parameter.x
  ret void
}

define private void @tests_generics_unconstrained.a.1(i32 %parameter.x) {
fn.entry:
  %int.add_op = add i32 %parameter.x, %parameter.x
  ret void
}

define private void @tests_generics_unconstrained.a.2(float %parameter.x) {
fn.entry:
  %float.add_op = fadd float %parameter.x, %parameter.x
  ret void
}
