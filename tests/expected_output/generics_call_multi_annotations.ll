; ModuleID = 'generics_call_multi_annotations'
source_filename = "generics_call_multi_annotations"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1

define private void @tests_generics_call_multi_annotations.tests() {
fn.entry:
  %object.alloca14 = alloca { i32 }, align 8
  %object.alloca9 = alloca { i32 }, align 8
  %object.alloca = alloca { i32 }, align 8
  %call = call i32 @tests_generics_call_multi_annotations.two(i32 123, i32 123)
  %call1 = call float @tests_generics_call_multi_annotations.two.1(float 3.140000e+02, i32 123)
  %call2 = call ptr @tests_generics_call_multi_annotations.two.2(ptr @string_literal, float 3.140000e+02)
  %call3 = call i1 @tests_generics_call_multi_annotations.two.3(i1 false, i1 true)
  %call4 = call i32 @tests_generics_call_multi_annotations.three(i32 123, i32 123, i32 123)
  %call5 = call i32 @tests_generics_call_multi_annotations.three.4(float 3.140000e+02, i32 123, ptr @string_literal)
  %call6 = call i1 @tests_generics_call_multi_annotations.three.5(i1 false, i1 true, ptr @string_literal)
  %object.alloca.field.gep = getelementptr inbounds { i32 }, ptr %object.alloca, i32 0, i32 0
  store i32 123, ptr %object.alloca.field.gep, align 4
  %access.object = load { i32 }, ptr %object.alloca, align 4
  %call7 = call float @tests_generics_call_multi_annotations.three.6({ i32 } %access.object, float 3.140000e+02, i1 true)
  %call8 = call i32 @tests_generics_call_multi_annotations.four(i32 123, i32 123, i32 123, i32 123)
  %object.alloca.field.gep10 = getelementptr inbounds { i32 }, ptr %object.alloca9, i32 0, i32 0
  store i32 123, ptr %object.alloca.field.gep10, align 4
  %access.object11 = load { i32 }, ptr %object.alloca9, align 4
  %call12 = call float @tests_generics_call_multi_annotations.four.7(i32 123, ptr @string_literal, float 3.140000e+02, { i32 } %access.object11)
  %call13 = call i1 @tests_generics_call_multi_annotations.four.8(ptr @string_literal, i1 false, i1 true, ptr @string_literal)
  %object.alloca.field.gep15 = getelementptr inbounds { i32 }, ptr %object.alloca14, i32 0, i32 0
  store i32 123, ptr %object.alloca.field.gep15, align 4
  %access.object16 = load { i32 }, ptr %object.alloca14, align 4
  %call17 = call { i32 } @tests_generics_call_multi_annotations.four.9(i32 123, float 3.140000e+02, { i32 } %access.object16, ptr @string_literal)
  ret void
}

define private i32 @tests_generics_call_multi_annotations.two(i32 %parameter.a, i32 %parameter.b) {
fn.entry:
  ret i32 %parameter.a
}

define private float @tests_generics_call_multi_annotations.two.1(float %parameter.a, i32 %parameter.b) {
fn.entry:
  ret float %parameter.a
}

define private ptr @tests_generics_call_multi_annotations.two.2(ptr %parameter.a, float %parameter.b) {
fn.entry:
  ret ptr %parameter.a
}

define private i1 @tests_generics_call_multi_annotations.two.3(i1 %parameter.a, i1 %parameter.b) {
fn.entry:
  ret i1 %parameter.a
}

define private i32 @tests_generics_call_multi_annotations.three(i32 %parameter.a, i32 %parameter.b, i32 %parameter.c) {
fn.entry:
  ret i32 %parameter.b
}

define private i32 @tests_generics_call_multi_annotations.three.4(float %parameter.a, i32 %parameter.b, ptr %parameter.c) {
fn.entry:
  ret i32 %parameter.b
}

define private i1 @tests_generics_call_multi_annotations.three.5(i1 %parameter.a, i1 %parameter.b, ptr %parameter.c) {
fn.entry:
  ret i1 %parameter.b
}

define private float @tests_generics_call_multi_annotations.three.6({ i32 } %parameter.a, float %parameter.b, i1 %parameter.c) {
fn.entry:
  ret float %parameter.b
}

define private i32 @tests_generics_call_multi_annotations.four(i32 %parameter.a, i32 %parameter.b, i32 %parameter.c, i32 %parameter.d) {
fn.entry:
  ret i32 %parameter.c
}

define private float @tests_generics_call_multi_annotations.four.7(i32 %parameter.a, ptr %parameter.b, float %parameter.c, { i32 } %parameter.d) {
fn.entry:
  ret float %parameter.c
}

define private i1 @tests_generics_call_multi_annotations.four.8(ptr %parameter.a, i1 %parameter.b, i1 %parameter.c, ptr %parameter.d) {
fn.entry:
  ret i1 %parameter.c
}

define private { i32 } @tests_generics_call_multi_annotations.four.9(i32 %parameter.a, float %parameter.b, { i32 } %parameter.c, ptr %parameter.d) {
fn.entry:
  ret { i32 } %parameter.c
}
