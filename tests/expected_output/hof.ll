; ModuleID = 'hof'
source_filename = "hof"

define private void @tests_hof.tests() {
fn.entry:
  %call = call i32 @tests_hof.hof()
  ret void
}

define private i32 @tests_hof.hof() {
fn.entry:
  ret i32 123
}
