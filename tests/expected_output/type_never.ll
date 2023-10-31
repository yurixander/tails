; ModuleID = 'type_never'
source_filename = "type_never"

define private void @tests_type_never.tests() {
fn.entry:
  call void @tests_type_never.a()
  ret void
}

define private void @tests_type_never.a() {
fn.entry:
  call void @abort()
  unreachable
}

declare void @abort()
