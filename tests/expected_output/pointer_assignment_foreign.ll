; ModuleID = 'tests.pointer_assignment_foreign'
source_filename = "tests.pointer_assignment_foreign"

@stdin = external global ptr

define private void @tests_pointer_assignment_foreign.tests() {
fn.entry:
  store i8 1, ptr @stdin, align 1
  ret void
}
