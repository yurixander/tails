; ModuleID = 'tests.unit_constant'
source_filename = "tests.unit_constant"

@tests_unit_constant.A = addrspace(4) global ptr null

define private void @tests_unit_constant.tests() {
fn.entry:
  %access.constant = load ptr, ptr addrspace(4) @tests_unit_constant.A, align 8
  ret void
}
