; ModuleID = 'tests.pointer'
source_filename = "tests.pointer"

@guard.message = private unnamed_addr constant [56 x i8] c"runtime assertion failed: dereference of a null pointer\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1

define private void @tests_pointer.tests() {
fn.entry:
  br i1 false, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %fn.entry
  %access.dereference_op = load i32, ptr null, align 4
  br i1 false, label %guard.continuation1, label %guard.failure

guard.failure:                                    ; preds = %guard.continuation17, %guard.continuation15, %guard.continuation13, %guard.continuation9, %guard.continuation5, %guard.continuation3, %guard.continuation1, %guard.continuation, %fn.entry
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable

guard.continuation1:                              ; preds = %guard.continuation
  %access.dereference_op2 = load ptr, ptr null, align 8
  %is_null = icmp eq ptr %access.dereference_op2, null
  %is_not_null = icmp eq i1 %is_null, false
  br i1 %is_not_null, label %guard.continuation3, label %guard.failure

guard.continuation3:                              ; preds = %guard.continuation1
  %access.dereference_op4 = load i32, ptr %access.dereference_op2, align 4
  br i1 false, label %guard.continuation5, label %guard.failure

guard.continuation5:                              ; preds = %guard.continuation3
  %access.dereference_op6 = load ptr, ptr null, align 8
  %is_null7 = icmp eq ptr %access.dereference_op6, null
  %is_not_null8 = icmp eq i1 %is_null7, false
  br i1 %is_not_null8, label %guard.continuation9, label %guard.failure

guard.continuation9:                              ; preds = %guard.continuation5
  %access.dereference_op10 = load ptr, ptr %access.dereference_op6, align 8
  %is_null11 = icmp eq ptr %access.dereference_op10, null
  %is_not_null12 = icmp eq i1 %is_null11, false
  br i1 %is_not_null12, label %guard.continuation13, label %guard.failure

guard.continuation13:                             ; preds = %guard.continuation9
  %access.dereference_op14 = load i32, ptr %access.dereference_op10, align 4
  br i1 false, label %guard.continuation15, label %guard.failure

guard.continuation15:                             ; preds = %guard.continuation13
  %access.dereference_op16 = load i32, ptr null, align 4
  br i1 false, label %guard.continuation17, label %guard.failure

guard.continuation17:                             ; preds = %guard.continuation15
  %access.dereference_op18 = load i32, ptr null, align 4
  br i1 false, label %guard.continuation19, label %guard.failure

guard.continuation19:                             ; preds = %guard.continuation17
  %access.dereference_op20 = load i32, ptr null, align 4
  %int.add_op = add i32 %access.dereference_op18, %access.dereference_op20
  ret void
}

declare i32 @puts(ptr)

; Function Attrs: noreturn
declare void @abort() #0

attributes #0 = { noreturn }
