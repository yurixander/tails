; ModuleID = 'binary_op_arithmetic'
source_filename = "binary_op_arithmetic"

@guard.message = private unnamed_addr constant [43 x i8] c"runtime assertion failed: division by zero\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1

define private void @tests_binary_op_arithmetic.tests() {
fn.entry:
  call void @tests_binary_op_arithmetic.id(i32 3)
  call void @tests_binary_op_arithmetic.id(i32 -1)
  call void @tests_binary_op_arithmetic.id(i32 2)
  br i1 true, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %fn.entry
  call void @tests_binary_op_arithmetic.id(i32 0)
  call void @tests_binary_op_arithmetic.id(i32 2)
  br i1 true, label %guard.continuation1, label %guard.failure

guard.failure:                                    ; preds = %guard.continuation3, %guard.continuation2, %guard.continuation1, %guard.continuation, %fn.entry
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable

guard.continuation1:                              ; preds = %guard.continuation
  call void @tests_binary_op_arithmetic.id(i32 3)
  br i1 true, label %guard.continuation2, label %guard.failure

guard.continuation2:                              ; preds = %guard.continuation1
  call void @tests_binary_op_arithmetic.id(i32 0)
  br i1 true, label %guard.continuation3, label %guard.failure

guard.continuation3:                              ; preds = %guard.continuation2
  br i1 true, label %guard.continuation4, label %guard.failure

guard.continuation4:                              ; preds = %guard.continuation3
  call void @tests_binary_op_arithmetic.id(i32 0)
  ret void
}

define private void @tests_binary_op_arithmetic.id(i32 %parameter.x) {
fn.entry:
  ret void
}

declare i32 @puts(ptr)

; Function Attrs: noreturn
declare void @abort() #0

attributes #0 = { noreturn }
