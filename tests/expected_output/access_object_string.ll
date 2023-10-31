; ModuleID = 'access_object_string'
source_filename = "access_object_string"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1

define private void @tests_access_object_string.tests() {
fn.entry:
  %object.alloca = alloca { ptr }, align 8
  %object.alloca.field.gep = getelementptr inbounds { ptr }, ptr %object.alloca, i32 0, i32 0
  store ptr @string_literal, ptr %object.alloca.field.gep, align 8
  %object.field.gep = getelementptr inbounds { ptr }, ptr %object.alloca, i32 0, i32 0
  %object.field.gep1 = getelementptr inbounds { ptr }, ptr %object.alloca, i32 0, i32 0
  %access.object.field.access = load ptr, ptr %object.field.gep1, align 8
  ret void
}
