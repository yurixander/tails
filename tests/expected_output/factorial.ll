; ModuleID = 'tests.factorial'
source_filename = "tests.factorial"

define private void @tests_factorial.tests() {
fn.entry:
  %call = call i32 @tests_factorial.factorial1(i32 3)
  %call1 = call i32 @tests_factorial.factorial2(i32 3, i32 1)
  ret void
}

define private i32 @tests_factorial.factorial1(i32 %parameter.n) {
fn.entry:
  %if.value = alloca i32, align 4
  %int.eq_op = icmp eq i32 %parameter.n, 0
  br i1 %int.eq_op, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  ret i32 %access.if.value

if.else:                                          ; preds = %fn.entry
  %int.subtract_op = sub i32 %parameter.n, 1
  %call = call i32 @tests_factorial.factorial1(i32 %int.subtract_op)
  %int.multiply_op = mul i32 %parameter.n, %call
  store i32 %int.multiply_op, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i32 1, ptr %if.value, align 4
  br label %if.after
}

define private i32 @tests_factorial.factorial2(i32 %parameter.n, i32 %parameter.acc) {
fn.entry:
  %if.value = alloca i32, align 4
  %int.eq_op = icmp eq i32 %parameter.n, 0
  br i1 %int.eq_op, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  ret i32 %access.if.value

if.else:                                          ; preds = %fn.entry
  %int.subtract_op = sub i32 %parameter.n, 1
  %int.multiply_op = mul i32 %parameter.n, %parameter.acc
  %call = call i32 @tests_factorial.factorial2(i32 %int.subtract_op, i32 %int.multiply_op)
  store i32 %call, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i32 %parameter.acc, ptr %if.value, align 4
  br label %if.after
}
