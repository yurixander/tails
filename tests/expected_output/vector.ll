; ModuleID = 'vector'
source_filename = "vector"

@tests_vector.NOT_FOUND = addrspace(4) global i32 -1
@guard.message = private unnamed_addr constant [56 x i8] c"runtime assertion failed: dereference of a null pointer\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1

define private void @tests_vector.tests() {
fn.entry:
  %call = call { i64, i64, ptr } @tests_vector.alloc(i64 3)
  %call1 = call { i64, i64, ptr } @tests_vector.alloc(i64 3)
  %call2 = call i1 @tests_vector.set({ i64, i64, ptr } %call1, i64 0, i32 123)
  %call3 = call i32 @tests_vector.get({ i64, i64, ptr } %call1, i64 0)
  ret void
}

define private { i64, i64, ptr } @tests_vector.alloc(i64 %parameter.capacity) {
fn.entry:
  %object.alloca = alloca { i64, i64, ptr }, align 8
  %int.multiply_op = mul i64 ptrtoint (ptr getelementptr (i32, ptr null, i32 1) to i64), %parameter.capacity
  %call = call ptr @malloc(i64 %int.multiply_op)
  %object.alloca.field.gep = getelementptr inbounds { i64, i64, ptr }, ptr %object.alloca, i32 0, i32 0
  store i64 %parameter.capacity, ptr %object.alloca.field.gep, align 4
  %object.alloca.field.gep1 = getelementptr inbounds { i64, i64, ptr }, ptr %object.alloca, i32 0, i32 1
  store i64 0, ptr %object.alloca.field.gep1, align 4
  %int.multiply_op2 = mul i64 ptrtoint (ptr getelementptr (i32, ptr null, i32 1) to i64), %parameter.capacity
  %call3 = call ptr @malloc(i64 %int.multiply_op2)
  %object.alloca.field.gep4 = getelementptr inbounds { i64, i64, ptr }, ptr %object.alloca, i32 0, i32 2
  store ptr %call3, ptr %object.alloca.field.gep4, align 8
  %access.object = load { i64, i64, ptr }, ptr %object.alloca, align 8
  ret { i64, i64, ptr } %access.object
}

declare ptr @malloc(i64)

define private i1 @tests_vector.set({ i64, i64, ptr } %parameter.vector, i64 %parameter.index, i32 %parameter.value) {
fn.entry:
  %if.value = alloca i1, align 1
  %object.field.extract = extractvalue { i64, i64, ptr } %parameter.vector, 1
  %int.slt_op = icmp slt i64 %parameter.index, %object.field.extract
  br i1 %int.slt_op, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  %access.if.value = load i1, ptr %if.value, align 1
  ret i1 %access.if.value

if.else:                                          ; preds = %fn.entry
  store i1 false, ptr %if.value, align 1
  br label %if.after

if.then:                                          ; preds = %fn.entry
  %object.field.extract1 = extractvalue { i64, i64, ptr } %parameter.vector, 2
  %pointer_indexing.gep = getelementptr i32, ptr %object.field.extract1, i64 %parameter.index
  store i32 %parameter.value, ptr %pointer_indexing.gep, align 4
  store i1 true, ptr %if.value, align 1
  br label %if.after
}

define private i32 @tests_vector.get({ i64, i64, ptr } %parameter.vector, i64 %parameter.index) {
fn.entry:
  %if.value = alloca i32, align 4
  %object.field.extract = extractvalue { i64, i64, ptr } %parameter.vector, 1
  %int.slt_op = icmp slt i64 %parameter.index, %object.field.extract
  br i1 %int.slt_op, label %if.then, label %if.else

if.after:                                         ; preds = %guard.continuation, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  ret i32 %access.if.value

if.else:                                          ; preds = %fn.entry
  %access.constant = load i32, ptr addrspace(4) @tests_vector.NOT_FOUND, align 4
  store i32 %access.constant, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  %object.field.extract1 = extractvalue { i64, i64, ptr } %parameter.vector, 2
  %pointer_indexing.gep = getelementptr i32, ptr %object.field.extract1, i64 %parameter.index
  %is_null = icmp eq ptr %pointer_indexing.gep, null
  %is_not_null = icmp eq i1 %is_null, false
  br i1 %is_not_null, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %if.then
  %access.dereference_op = load i32, ptr %pointer_indexing.gep, align 4
  store i32 %access.dereference_op, ptr %if.value, align 4
  br label %if.after

guard.failure:                                    ; preds = %if.then
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable
}

declare i32 @puts(ptr)

; Function Attrs: noreturn
declare void @abort() #0

attributes #0 = { noreturn }
