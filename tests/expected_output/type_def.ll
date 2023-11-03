; ModuleID = 'tests.type_def'
source_filename = "tests.type_def"

@string_literal = private unnamed_addr constant [5 x i8] c"john\00", align 1

define private void @tests_type_def.tests() {
fn.entry:
  %object.alloca = alloca { i32, ptr }, align 8
  %object.alloca.field.gep = getelementptr inbounds { i32, ptr }, ptr %object.alloca, i32 0, i32 0
  store i32 42, ptr %object.alloca.field.gep, align 4
  %object.alloca.field.gep1 = getelementptr inbounds { i32, ptr }, ptr %object.alloca, i32 0, i32 1
  store ptr @string_literal, ptr %object.alloca.field.gep1, align 8
  %call = call i32 @tests_type_def.a()
  %call2 = call i32 @tests_type_def.b()
  call void @tests_type_def.c(i32 0)
  %call3 = call i32 @tests_type_def.d()
  %call4 = call i32 @tests_type_def.e(i32 0)
  %call5 = call { i32, ptr } @tests_type_def.person()
  ret void
}

define private i32 @tests_type_def.a() {
fn.entry:
  ret i32 123
}

define private i32 @tests_type_def.b() {
fn.entry:
  ret i32 123
}

define private void @tests_type_def.c(i32 %parameter.x) {
fn.entry:
  ret void
}

define private i32 @tests_type_def.d() {
fn.entry:
  ret i32 321
}

define private i32 @tests_type_def.e(i32 %parameter.x) {
fn.entry:
  ret i32 321
}

define private { i32, ptr } @tests_type_def.person() {
fn.entry:
  %object.alloca = alloca { i32, ptr }, align 8
  %object.alloca.field.gep = getelementptr inbounds { i32, ptr }, ptr %object.alloca, i32 0, i32 0
  store i32 42, ptr %object.alloca.field.gep, align 4
  %object.alloca.field.gep1 = getelementptr inbounds { i32, ptr }, ptr %object.alloca, i32 0, i32 1
  store ptr @string_literal, ptr %object.alloca.field.gep1, align 8
  %access.object = load { i32, ptr }, ptr %object.alloca, align 8
  ret { i32, ptr } %access.object
}
