; ModuleID = 'closure_capture'
source_filename = "closure_capture"

define private void @tests_closure_capture.tests() {
fn.entry:
  %call = call ptr @tests_closure_capture.g()
  %call1 = call i32 %call()
  ret void
}

define private ptr @tests_closure_capture.g() {
fn.entry:
  ret ptr @tests_closure_capture.closure
}

define private i32 @tests_closure_capture.closure(ptr %closure.environment) {
closure.entry:
  %closure.capture.x = getelementptr inbounds { i32 }, ptr %closure.environment, i32 0, i32 0
  %access.closure.capture = load i32, ptr %closure.capture.x, align 4
  ret i32 %access.closure.capture
}
