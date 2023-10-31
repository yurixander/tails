; ModuleID = 'guard_memo'
source_filename = "guard_memo"

@guard.message = private unnamed_addr constant [43 x i8] c"runtime assertion failed: division by zero\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1
@guard.message.1 = private unnamed_addr constant [56 x i8] c"runtime assertion failed: dereference of a null pointer\00", align 1

define private void @tests_guard_memo.tests() {
fn.entry:
  br i1 false, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %fn.entry
  br i1 true, label %guard.continuation1, label %guard.failure

guard.failure:                                    ; preds = %guard.continuation1, %guard.continuation, %fn.entry
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable

guard.continuation1:                              ; preds = %guard.continuation
  br i1 true, label %guard.continuation2, label %guard.failure

guard.continuation2:                              ; preds = %guard.continuation1
  br i1 false, label %guard.continuation3, label %guard.failure4

guard.continuation3:                              ; preds = %guard.continuation2
  %access.dereference_op = load i32, ptr null, align 4
  br i1 false, label %guard.continuation7, label %guard.failure4

guard.failure4:                                   ; preds = %guard.continuation3, %guard.continuation2
  %guard.puts5 = call i32 @puts(ptr @guard.message.1)
  %guard.note.puts6 = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable

guard.continuation7:                              ; preds = %guard.continuation3
  %access.dereference_op8 = load float, ptr null, align 4
  ret void
}

declare i32 @puts(ptr)

; Function Attrs: noreturn
declare void @abort() #0

attributes #0 = { noreturn }
