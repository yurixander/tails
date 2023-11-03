; ModuleID = 'tests.reference_object'
source_filename = "tests.reference_object"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1

define private void @tests_reference_object.tests() {
fn.entry:
  %object.alloca = alloca { i32, ptr }, align 8
  %object.alloca.field.gep = getelementptr inbounds { i32, ptr }, ptr %object.alloca, i32 0, i32 0
  store i32 123, ptr %object.alloca.field.gep, align 4
  %object.alloca.field.gep1 = getelementptr inbounds { i32, ptr }, ptr %object.alloca, i32 0, i32 1
  store ptr @string_literal, ptr %object.alloca.field.gep1, align 8
  call void @tests_reference_object.receive(ptr %object.alloca)
  ret void
}

define private void @tests_reference_object.receive(ptr %parameter.a) {
fn.entry:
  ret void
}
