; ModuleID = 'tests.pointer_index'
source_filename = "tests.pointer_index"

@guard.message = private unnamed_addr constant [56 x i8] c"runtime assertion failed: dereference of a null pointer\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1

define private void @tests_pointer_index.tests() {
fn.entry:
  br i1 false, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %fn.entry
  %access.dereference_op = load i32, ptr null, align 4
  br i1 false, label %guard.continuation1, label %guard.failure

guard.failure:                                    ; preds = %guard.continuation, %fn.entry
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable

guard.continuation1:                              ; preds = %guard.continuation
  %access.dereference_op2 = load i32, ptr null, align 4
  call void @tests_pointer_index.receiver(ptr null, ptr null, i32 %access.dereference_op2)
  ret void
}

declare i32 @puts(ptr)

; Function Attrs: noreturn
declare void @abort() #0

define private void @tests_pointer_index.receiver(ptr %parameter.a, ptr %parameter.b, i32 %parameter.c) {
fn.entry:
  ret void
}

attributes #0 = { noreturn }
