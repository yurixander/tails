; ModuleID = 'tests.unit_closure_capture'
source_filename = "tests.unit_closure_capture"

define private void @tests_unit_closure_capture.tests() {
fn.entry:
  call void @"tests_unit_closure_capture.a'"(ptr null)
  ret void
}

define private void @tests_unit_closure_capture.closure(ptr %closure.environment) {
closure.entry:
  %closure.capture.a = getelementptr inbounds { ptr }, ptr %closure.environment, i32 0, i32 0
  %access.closure.capture = load ptr, ptr %closure.capture.a, align 8
  ret void
}

define private void @"tests_unit_closure_capture.a'"(ptr %parameter.x) {
fn.entry:
  ret void
}
