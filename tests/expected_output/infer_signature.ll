; ModuleID = 'infer_signature'
source_filename = "infer_signature"

define private void @tests_infer_signature.tests() {
fn.entry:
  call void @tests_infer_signature.a(ptr null)
  ret void
}

define private void @tests_infer_signature.a(ptr %parameter.x) {
fn.entry:
  ret void
}
