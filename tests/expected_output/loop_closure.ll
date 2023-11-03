; ModuleID = 'tests.loop_closure'
source_filename = "tests.loop_closure"

define private void @tests_loop_closure.tests() {
fn.entry:
  call void @tests_loop_closure.loopClosure(i32 100, ptr @tests_loop_closure.closure)
  ret void
}

define private void @tests_loop_closure.closure(i32 %count) {
closure.entry:
  ret void
}

define private void @tests_loop_closure.loopClosure(i32 %parameter.count, ptr %parameter.callback) {
fn.entry:
  call void %parameter.callback(i32 %parameter.count)
  %int.eq_op = icmp eq i32 %parameter.count, 0
  br i1 %int.eq_op, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  ret void

if.else:                                          ; preds = %fn.entry
  %int.subtract_op = sub i32 %parameter.count, 1
  call void @tests_loop_closure.loopClosure(i32 %int.subtract_op, ptr %parameter.callback)
  br label %if.after

if.then:                                          ; preds = %fn.entry
  br label %if.after
}
