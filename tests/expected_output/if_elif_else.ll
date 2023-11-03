; ModuleID = 'tests.if_elif_else'
source_filename = "tests.if_elif_else"

define private void @tests_if_elif_else.tests() {
fn.entry:
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.elif.check

if.after:                                         ; preds = %if.elif.then2, %if.elif.then, %if.then, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  ret void

if.elif.check:                                    ; preds = %fn.entry
  br i1 false, label %if.elif.then, label %if.elif.check1

if.elif.check1:                                   ; preds = %if.elif.check
  br i1 true, label %if.elif.then2, label %if.else

if.else:                                          ; preds = %if.elif.check1
  store i32 6, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i32 3, ptr %if.value, align 4
  br label %if.after

if.elif.then:                                     ; preds = %if.elif.check
  store i32 4, ptr %if.value, align 4
  br label %if.after

if.elif.then2:                                    ; preds = %if.elif.check1
  store i32 5, ptr %if.value, align 4
  br label %if.after
}
