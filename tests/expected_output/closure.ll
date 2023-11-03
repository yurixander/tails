; ModuleID = 'tests.closure'
source_filename = "tests.closure"

define private void @tests_closure.tests() {
fn.entry:
  %call = call i32 @tests_closure.closure(i32 0)
  %call1 = call i32 @tests_closure.closure.2()
  %call2 = call i32 @tests_closure.closure.1()
  %call3 = call i32 @tests_closure.closure(i32 %call2)
  ret void
}

define private i32 @tests_closure.closure(i32 %n) {
closure.entry:
  %int.multiply_op = mul i32 %n, 2
  ret i32 %int.multiply_op
}

define private i32 @tests_closure.closure.1() {
closure.entry:
  ret i32 0
}

define private i32 @tests_closure.closure.2() {
closure.entry:
  ret i32 1222
}
