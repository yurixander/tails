; ModuleID = 'foreign_var_object_type'
source_filename = "foreign_var_object_type"

@a = external global ptr
@guard.message = private unnamed_addr constant [56 x i8] c"runtime assertion failed: dereference of a null pointer\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1

define private void @tests_foreign_var_object_type.tests() {
fn.entry:
  br i1 true, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %fn.entry
  %access.dereference_op = load { float, i32 }, ptr @a, align 4
  ret void

guard.failure:                                    ; preds = %fn.entry
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable
}

declare i32 @puts(ptr)

; Function Attrs: noreturn
declare void @abort() #0

attributes #0 = { noreturn }
