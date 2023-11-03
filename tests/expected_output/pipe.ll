; ModuleID = 'tests.pipe'
source_filename = "tests.pipe"

define private void @tests_pipe.tests() {
fn.entry:
  %call = call i32 @tests_pipe.id(i32 123)
  ret void
}

define private i32 @tests_pipe.id(i32 %parameter.x) {
fn.entry:
  ret i32 %parameter.x
}
