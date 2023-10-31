; ModuleID = 'block'
source_filename = "block"

define private void @tests_block.tests() {
fn.entry:
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  ret void

if.else:                                          ; preds = %fn.entry
  store i32 2, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i32 1, ptr %if.value, align 4
  br label %if.after
}
