; ModuleID = 'tests.declare'
source_filename = "tests.declare"

define private void @tests_declare.tests() {
fn.entry:
  %call = call i1 @tests_declare.first(i1 true)
  %call1 = call i32 @tests_declare.second(i32 1)
  ret void
}

define private i1 @tests_declare.first(i1 %parameter.a) {
fn.entry:
  ret i1 %parameter.a
}

define private i32 @tests_declare.second(i32 %parameter.a) {
fn.entry:
  %int.add_op = add i32 %parameter.a, %parameter.a
  ret i32 %int.add_op
}
