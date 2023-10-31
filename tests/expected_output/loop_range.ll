; ModuleID = 'loop_range'
source_filename = "loop_range"

define private void @tests_loop_range.tests() {
fn.entry:
  call void @tests_loop_range.loopRange(i32 0, i32 100)
  ret void
}

define private void @tests_loop_range.loopRange(i32 %parameter.i, i32 %parameter.end_) {
fn.entry:
  %int.sltoe_op = icmp sle i32 1, %parameter.end_
  br i1 %int.sltoe_op, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  ret void

if.else:                                          ; preds = %fn.entry
  call void @tests_loop_range.loopRange(i32 1, i32 %parameter.end_)
  br label %if.after

if.then:                                          ; preds = %fn.entry
  br label %if.after
}
