; ModuleID = 'fibonacci'
source_filename = "fibonacci"

define private void @tests_fibonacci.tests() {
fn.entry:
  %call = call i32 @tests_fibonacci.fibonacci(i32 3)
  ret void
}

define private i32 @tests_fibonacci.fibonacci(i32 %parameter.n) {
fn.entry:
  %if.value = alloca i32, align 4
  %int.eq_op = icmp eq i32 %parameter.n, 0
  br i1 %int.eq_op, label %if.then, label %if.elif.check

if.after:                                         ; preds = %if.elif.then, %if.then, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  ret i32 %access.if.value

if.elif.check:                                    ; preds = %fn.entry
  %int.eq_op3 = icmp eq i32 %parameter.n, 1
  br i1 %int.eq_op3, label %if.elif.then, label %if.else

if.else:                                          ; preds = %if.elif.check
  %int.subtract_op = sub i32 %parameter.n, 1
  %call = call i32 @tests_fibonacci.fibonacci(i32 %int.subtract_op)
  %int.subtract_op1 = sub i32 %parameter.n, 2
  %call2 = call i32 @tests_fibonacci.fibonacci(i32 %int.subtract_op1)
  %int.add_op = add i32 %call, %call2
  store i32 %int.add_op, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i32 0, ptr %if.value, align 4
  br label %if.after

if.elif.then:                                     ; preds = %if.elif.check
  store i32 1, ptr %if.value, align 4
  br label %if.after
}
