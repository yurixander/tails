; ModuleID = 'tests.generics_constrain'
source_filename = "tests.generics_constrain"

@guard.message = private unnamed_addr constant [56 x i8] c"runtime assertion failed: dereference of a null pointer\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1

define private void @tests_generics_constrain.tests() {
fn.entry:
  %object.alloca = alloca { i32 }, align 8
  call void @tests_generics_constrain.constrainBool(i1 true)
  call void @tests_generics_constrain.constrainInt(i32 123)
  %call = call i32 @tests_generics_constrain.constrainPointer(ptr null)
  %object.alloca.field.gep = getelementptr inbounds { i32 }, ptr %object.alloca, i32 0, i32 0
  store i32 0, ptr %object.alloca.field.gep, align 4
  %access.object = load { i32 }, ptr %object.alloca, align 4
  call void @tests_generics_constrain.constrainObject({ i32 } %access.object)
  ret void
}

define private void @tests_generics_constrain.constrainBool(i1 %parameter.x) {
fn.entry:
  %not_op = xor i1 %parameter.x, true
  ret void
}

define private void @tests_generics_constrain.constrainInt(i32 %parameter.x) {
fn.entry:
  %int.add_op = add i32 %parameter.x, %parameter.x
  ret void
}

define private i32 @tests_generics_constrain.constrainPointer(ptr %parameter.x) {
fn.entry:
  %is_null = icmp eq ptr %parameter.x, null
  %is_not_null = icmp eq i1 %is_null, false
  br i1 %is_not_null, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %fn.entry
  %access.dereference_op = load i32, ptr %parameter.x, align 4
  ret i32 %access.dereference_op

guard.failure:                                    ; preds = %fn.entry
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable
}

declare i32 @puts(ptr)

declare void @abort()

define private void @tests_generics_constrain.constrainObject({ i32 } %parameter.x) {
fn.entry:
  %object.field.extract = extractvalue { i32 } %parameter.x, 0
  ret void
}
