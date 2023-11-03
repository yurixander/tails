; ModuleID = 'tests.binding'
source_filename = "tests.binding"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1
@string_literal.3 = private unnamed_addr constant [3 x i8] c"hi\00", align 1

define private void @tests_binding.tests() {
fn.entry:
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  call void @tests_binding.id(i32 1)
  call void @tests_binding.id(i32 1)
  call void @tests_binding.id(i32 2)
  call void @tests_binding.id.2(ptr null)
  call void @tests_binding.id.2(ptr null)
  call void @tests_binding.id.4(ptr @string_literal.3)
  call void @tests_binding.id(i32 123)
  call void @tests_binding.id.5(i1 false)
  call void @tests_binding.id.6(ptr @tests_binding.closure)
  call void @tests_binding.id.7(ptr @tests_binding.closure.1)
  call void @tests_binding.id(i32 1)
  call void @tests_binding.id(i32 1)
  call void @tests_binding.id.2(ptr null)
  ret void

if.else:                                          ; preds = %fn.entry
  store i32 1, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i32 1, ptr %if.value, align 4
  br label %if.after
}

define private i32 @tests_binding.closure() {
closure.entry:
  ret i32 0
}

define private void @tests_binding.closure.1() {
closure.entry:
  ret void
}

define private void @tests_binding.id(i32 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding.id.2(ptr %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding.id.4(ptr %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding.id.5(i1 %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding.id.6(ptr %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_binding.id.7(ptr %parameter.x) {
fn.entry:
  ret void
}
