; ModuleID = 'type_infer_parameter'
source_filename = "type_infer_parameter"

@string_literal = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@string_literal.1 = private unnamed_addr constant [6 x i8] c"world\00", align 1

define private void @tests_type_infer_parameter.tests() {
fn.entry:
  %object.alloca2 = alloca { i32, float }, align 8
  %object.alloca = alloca { i32, float }, align 8
  call void @tests_type_infer_parameter.a(i32 123)
  call void @tests_type_infer_parameter.a(i32 321)
  call void @tests_type_infer_parameter.b(float 3.140000e+02, ptr @string_literal)
  call void @tests_type_infer_parameter.b(float 1.233210e+05, ptr @string_literal.1)
  %object.alloca.field.gep = getelementptr inbounds { i32, float }, ptr %object.alloca, i32 0, i32 0
  store i32 123, ptr %object.alloca.field.gep, align 4
  %object.alloca.field.gep1 = getelementptr inbounds { i32, float }, ptr %object.alloca, i32 0, i32 1
  store float 3.140000e+02, ptr %object.alloca.field.gep1, align 4
  %access.object = load { i32, float }, ptr %object.alloca, align 4
  call void @tests_type_infer_parameter.c({ i32, float } %access.object)
  %object.alloca.field.gep3 = getelementptr inbounds { i32, float }, ptr %object.alloca2, i32 0, i32 0
  store i32 321, ptr %object.alloca.field.gep3, align 4
  %object.alloca.field.gep4 = getelementptr inbounds { i32, float }, ptr %object.alloca2, i32 0, i32 1
  store float 1.233210e+05, ptr %object.alloca.field.gep4, align 4
  %access.object5 = load { i32, float }, ptr %object.alloca2, align 4
  call void @tests_type_infer_parameter.c({ i32, float } %access.object5)
  ret void
}

define private void @tests_type_infer_parameter.a(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_type_infer_parameter.b(float %parameter.x, ptr %parameter.y) {
fn.entry:
  ret void
}

define private void @tests_type_infer_parameter.c({ i32, float } %parameter.x) {
fn.entry:
  ret void
}
