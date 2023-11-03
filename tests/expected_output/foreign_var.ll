; ModuleID = 'tests.foreign_var'
source_filename = "tests.foreign_var"

@stdout = external global ptr
@stdin = external global ptr
@stderr = external global ptr

define private void @tests_foreign_var.tests() {
fn.entry:
  ret void
}
