; ModuleID = 'expr'
source_filename = "expr"

@guard.message = private unnamed_addr constant [43 x i8] c"runtime assertion failed: division by zero\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1

define private void @tests_expr.tests() {
fn.entry:
  br i1 true, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %fn.entry
  ret void

guard.failure:                                    ; preds = %fn.entry
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable
}

declare i32 @puts(ptr)

declare void @abort()
