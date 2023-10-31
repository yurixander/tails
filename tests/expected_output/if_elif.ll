; ModuleID = 'if_elif'
source_filename = "if_elif"

define private void @tests_if_elif.tests() {
fn.entry:
  br i1 true, label %if.then, label %if.elif.check

if.after:                                         ; preds = %if.elif.then, %if.elif.check, %if.then
  ret void

if.elif.check:                                    ; preds = %fn.entry
  br i1 false, label %if.elif.then, label %if.after

if.then:                                          ; preds = %fn.entry
  br label %if.after

if.elif.then:                                     ; preds = %if.elif.check
  br label %if.after
}
