; ModuleID = 'tests.if_nested_condition'
source_filename = "tests.if_nested_condition"

define private void @tests_if_nested_condition.tests() {
fn.entry:
  %if.value13 = alloca i1, align 1
  %if.value6 = alloca i1, align 1
  %if.value1 = alloca i32, align 4
  %if.value = alloca i1, align 1
  br i1 true, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  %access.if.value = load i1, ptr %if.value, align 1
  br i1 %access.if.value, label %if.then4, label %if.else3

if.else:                                          ; preds = %fn.entry
  store i1 false, ptr %if.value, align 1
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i1 true, ptr %if.value, align 1
  br label %if.after

if.after2:                                        ; preds = %if.then4, %if.else3
  %access.if.value5 = load i32, ptr %if.value1, align 4
  br i1 true, label %if.then9, label %if.else8

if.else3:                                         ; preds = %if.after
  store i32 0, ptr %if.value1, align 4
  br label %if.after2

if.then4:                                         ; preds = %if.after
  store i32 1, ptr %if.value1, align 4
  br label %if.after2

if.after7:                                        ; preds = %if.then9, %if.else8
  %access.if.value10 = load i1, ptr %if.value6, align 1
  br i1 %access.if.value10, label %if.then12, label %if.after11

if.else8:                                         ; preds = %if.after2
  store i1 false, ptr %if.value6, align 1
  br label %if.after7

if.then9:                                         ; preds = %if.after2
  store i1 true, ptr %if.value6, align 1
  br label %if.after7

if.after11:                                       ; preds = %if.after7, %if.then12
  br i1 true, label %if.then16, label %if.else15

if.then12:                                        ; preds = %if.after7
  br label %if.after11

if.after14:                                       ; preds = %if.then16, %if.else15
  %access.if.value17 = load i1, ptr %if.value13, align 1
  br i1 %access.if.value17, label %if.then20, label %if.else19

if.else15:                                        ; preds = %if.after11
  store i1 false, ptr %if.value13, align 1
  br label %if.after14

if.then16:                                        ; preds = %if.after11
  store i1 true, ptr %if.value13, align 1
  br label %if.after14

if.after18:                                       ; preds = %if.then20, %if.else19
  ret void

if.else19:                                        ; preds = %if.after14
  br label %if.after18

if.then20:                                        ; preds = %if.after14
  br label %if.after18
}
