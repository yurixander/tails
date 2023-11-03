; ModuleID = 'tests.access'
source_filename = "tests.access"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1

define private void @tests_access.tests() {
fn.entry:
  ret void
}
