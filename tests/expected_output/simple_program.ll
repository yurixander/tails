; ModuleID = 'tests.simple_program'
source_filename = "tests.simple_program"

define private void @tests_simple_program.tests() {
fn.entry:
  %call = call i32 @main(i32 0, ptr null)
  ret void
}

define i32 @main(i32 %parameter.argc, ptr %parameter.argv) {
fn.entry:
  ret i32 0
}
