; ModuleID = 'tests.access_foreign_var'
source_filename = "tests.access_foreign_var"

@stdin = external global ptr
@guard.message = private unnamed_addr constant [56 x i8] c"runtime assertion failed: dereference of a null pointer\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1
@stdin.1 = external global ptr

define private void @tests_access_foreign_var.tests() {
fn.entry:
  br i1 true, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %fn.entry
  %access.dereference_op = load i8, ptr @stdin, align 1
  call void @tests_access_foreign_var.a(ptr @stdin.1)
  br i1 true, label %guard.continuation1, label %guard.failure

guard.failure:                                    ; preds = %guard.continuation, %fn.entry
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable

guard.continuation1:                              ; preds = %guard.continuation
  %access.dereference_op2 = load i8, ptr @stdin, align 1
  call void @tests_access_foreign_var.a.2(i8 %access.dereference_op2)
  ret void
}

declare i32 @puts(ptr)

; Function Attrs: noreturn
declare void @abort() #0

define private void @tests_access_foreign_var.a(ptr %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_access_foreign_var.a.2(i8 %parameter.x) {
fn.entry:
  ret void
}

attributes #0 = { noreturn }
