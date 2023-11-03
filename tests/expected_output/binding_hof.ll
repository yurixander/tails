; ModuleID = 'tests.binding_hof'
source_filename = "tests.binding_hof"

define private void @tests_binding_hof.tests() {
fn.entry:
  %call = call i32 @tests_binding_hof.hof1()
  %call1 = call i32 @tests_binding_hof.hof1()
  %call2 = call i32 @tests_binding_hof.hof2(i32 1)
  %call3 = call i32 @tests_binding_hof.hof2(i32 1)
  ret void
}

define private i32 @tests_binding_hof.hof1() {
fn.entry:
  ret i32 123
}

define private i32 @tests_binding_hof.hof2(i32 %parameter.x) {
fn.entry:
  ret i32 %parameter.x
}
