; ModuleID = 'hof_return'
source_filename = "hof_return"

define private void @tests_hof_return.tests() {
fn.entry:
  %call = call ptr @tests_hof_return.a()
  call void @tests_hof_return.id(ptr %call)
  %call1 = call ptr @tests_hof_return.a()
  %call2 = call i32 %call1()
  call void @tests_hof_return.id.1(i32 %call2)
  ret void
}

define private ptr @tests_hof_return.a() {
fn.entry:
  ret ptr @tests_hof_return.closure
}

define private i32 @tests_hof_return.closure() {
closure.entry:
  ret i32 123
}

define private void @tests_hof_return.id(ptr %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_hof_return.id.1(i32 %parameter.x) {
fn.entry:
  ret void
}
