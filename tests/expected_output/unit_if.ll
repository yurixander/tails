; ModuleID = 'tests.unit_if'
source_filename = "tests.unit_if"

define private void @tests_unit_if.tests() {
fn.entry:
  br i1 true, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  br i1 true, label %if.then3, label %if.else2

if.else:                                          ; preds = %fn.entry
  br label %if.after

if.then:                                          ; preds = %fn.entry
  br label %if.after

if.after1:                                        ; preds = %if.then3, %if.else2
  br i1 true, label %if.then6, label %if.else5

if.else2:                                         ; preds = %if.after
  br label %if.after1

if.then3:                                         ; preds = %if.after
  br label %if.after1

if.after4:                                        ; preds = %if.then6, %if.else5
  br i1 true, label %if.then9, label %if.else8

if.else5:                                         ; preds = %if.after1
  br label %if.after4

if.then6:                                         ; preds = %if.after1
  br label %if.after4

if.after7:                                        ; preds = %if.then9, %if.else8
  call void @tests_unit_if.unbox(ptr null)
  br i1 true, label %if.then12, label %if.else11

if.else8:                                         ; preds = %if.after4
  br label %if.after7

if.then9:                                         ; preds = %if.after4
  br label %if.after7

if.after10:                                       ; preds = %if.then12, %if.else11
  call void @tests_unit_if.unbox(ptr null)
  br i1 true, label %if.then15, label %if.else14

if.else11:                                        ; preds = %if.after7
  br label %if.after10

if.then12:                                        ; preds = %if.after7
  br label %if.after10

if.after13:                                       ; preds = %if.then15, %if.else14
  call void @tests_unit_if.unbox(ptr null)
  ret void

if.else14:                                        ; preds = %if.after10
  br label %if.after13

if.then15:                                        ; preds = %if.after10
  br label %if.after13
}

define private void @tests_unit_if.unbox(ptr %parameter.a) {
fn.entry:
  ret void
}
