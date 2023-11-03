; ModuleID = 'tests.if_'
source_filename = "tests.if_"

define private void @tests_if_.tests() {
fn.entry:
  %if.value31 = alloca i32, align 4
  %if.value26 = alloca i32, align 4
  %if.value21 = alloca i32, align 4
  %if.value16 = alloca i32, align 4
  %if.value11 = alloca i32, align 4
  %if.value6 = alloca i32, align 4
  %if.value1 = alloca i32, align 4
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  br i1 false, label %if.then4, label %if.else3

if.else:                                          ; preds = %fn.entry
  store i32 2, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i32 1, ptr %if.value, align 4
  br label %if.after

if.after2:                                        ; preds = %if.then4, %if.else3
  %access.if.value5 = load i32, ptr %if.value1, align 4
  br i1 true, label %if.then9, label %if.else8

if.else3:                                         ; preds = %if.after
  store i32 2, ptr %if.value1, align 4
  br label %if.after2

if.then4:                                         ; preds = %if.after
  store i32 1, ptr %if.value1, align 4
  br label %if.after2

if.after7:                                        ; preds = %if.then9, %if.else8
  %access.if.value10 = load i32, ptr %if.value6, align 4
  br i1 true, label %if.then14, label %if.else13

if.else8:                                         ; preds = %if.after2
  store i32 2, ptr %if.value6, align 4
  br label %if.after7

if.then9:                                         ; preds = %if.after2
  store i32 1, ptr %if.value6, align 4
  br label %if.after7

if.after12:                                       ; preds = %if.then14, %if.else13
  %access.if.value15 = load i32, ptr %if.value11, align 4
  br i1 true, label %if.then19, label %if.else18

if.else13:                                        ; preds = %if.after7
  store i32 2, ptr %if.value11, align 4
  br label %if.after12

if.then14:                                        ; preds = %if.after7
  store i32 1, ptr %if.value11, align 4
  br label %if.after12

if.after17:                                       ; preds = %if.then19, %if.else18
  %access.if.value20 = load i32, ptr %if.value16, align 4
  br i1 true, label %if.then24, label %if.else23

if.else18:                                        ; preds = %if.after12
  store i32 2, ptr %if.value16, align 4
  br label %if.after17

if.then19:                                        ; preds = %if.after12
  store i32 1, ptr %if.value16, align 4
  br label %if.after17

if.after22:                                       ; preds = %if.then24, %if.else23
  %access.if.value25 = load i32, ptr %if.value21, align 4
  br i1 true, label %if.then29, label %if.else28

if.else23:                                        ; preds = %if.after17
  store i32 2, ptr %if.value21, align 4
  br label %if.after22

if.then24:                                        ; preds = %if.after17
  store i32 1, ptr %if.value21, align 4
  br label %if.after22

if.after27:                                       ; preds = %if.then29, %if.else28
  %access.if.value30 = load i32, ptr %if.value26, align 4
  br i1 true, label %if.then34, label %if.else33

if.else28:                                        ; preds = %if.after22
  store i32 2, ptr %if.value26, align 4
  br label %if.after27

if.then29:                                        ; preds = %if.after22
  store i32 1, ptr %if.value26, align 4
  br label %if.after27

if.after32:                                       ; preds = %if.then34, %if.else33
  %access.if.value35 = load i32, ptr %if.value31, align 4
  ret void

if.else33:                                        ; preds = %if.after27
  store i32 2, ptr %if.value31, align 4
  br label %if.after32

if.then34:                                        ; preds = %if.after27
  store i32 1, ptr %if.value31, align 4
  br label %if.after32
}
