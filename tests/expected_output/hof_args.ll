; ModuleID = 'tests.hof_args'
source_filename = "tests.hof_args"

define private void @tests_hof_args.tests() {
fn.entry:
  %call = call i32 @tests_hof_args.hof1(i32 1)
  %call1 = call i32 @tests_hof_args.hof2(i32 1, i32 2)
  ret void
}

define private i32 @tests_hof_args.hof1(i32 %parameter.x) {
fn.entry:
  %int.add_op = add i32 %parameter.x, 1
  ret i32 %int.add_op
}

define private i32 @tests_hof_args.hof2(i32 %parameter.a, i32 %parameter.b) {
fn.entry:
  %int.add_op = add i32 %parameter.a, %parameter.b
  ret i32 %int.add_op
}
