; ModuleID = 'pointer_assignment'
source_filename = "pointer_assignment"

define private void @tests_pointer_assignment.tests() {
fn.entry:
  store i32 1, ptr null, align 4
  ret void
}
