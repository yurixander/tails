; ModuleID = 'tests.loop_'
source_filename = "tests.loop_"

define private void @tests_loop_.tests() {
fn.entry:
  call void @tests_loop_.loop(i32 0)
  ret void
}

define private void @tests_loop_.loop(i32 %parameter.i) {
fn.entry:
  %int.eq_op = icmp eq i32 %parameter.i, 0
  br i1 %int.eq_op, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  ret void

if.else:                                          ; preds = %fn.entry
  %int.subtract_op = sub i32 %parameter.i, 1
  call void @tests_loop_.loop(i32 %int.subtract_op)
  br label %if.after

if.then:                                          ; preds = %fn.entry
  br label %if.after
}
