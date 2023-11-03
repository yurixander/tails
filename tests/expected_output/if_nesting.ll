; ModuleID = 'tests.if_nesting'
source_filename = "tests.if_nesting"

define private void @tests_if_nesting.tests() {
fn.entry:
  %if.value64 = alloca i32, align 4
  %if.value58 = alloca i32, align 4
  %if.value55 = alloca i32, align 4
  %if.value50 = alloca i1, align 1
  %if.value44 = alloca i32, align 4
  %if.value40 = alloca i32, align 4
  %if.value35 = alloca i1, align 1
  %if.value30 = alloca i32, align 4
  %if.value25 = alloca i1, align 1
  %if.value20 = alloca i32, align 4
  %if.value15 = alloca i1, align 1
  %if.value9 = alloca i32, align 4
  %if.value4 = alloca i32, align 4
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.after

if.after:                                         ; preds = %fn.entry, %if.after1
  br i1 true, label %if.then8, label %if.else

if.then:                                          ; preds = %fn.entry
  br i1 true, label %if.then2, label %if.after1

if.after1:                                        ; preds = %if.then, %if.then2
  br label %if.after

if.then2:                                         ; preds = %if.then
  br label %if.after1

if.after3:                                        ; preds = %if.after10, %if.after5
  %access.if.value14 = load i32, ptr %if.value, align 4
  br i1 true, label %if.then18, label %if.else17

if.else:                                          ; preds = %if.after
  br i1 false, label %if.then7, label %if.else6

if.after5:                                        ; preds = %if.then7, %if.else6
  %access.if.value = load i32, ptr %if.value4, align 4
  store i32 %access.if.value, ptr %if.value, align 4
  br label %if.after3

if.else6:                                         ; preds = %if.else
  store i32 1, ptr %if.value4, align 4
  br label %if.after5

if.then7:                                         ; preds = %if.else
  store i32 0, ptr %if.value4, align 4
  br label %if.after5

if.then8:                                         ; preds = %if.after
  br i1 true, label %if.then12, label %if.else11

if.after10:                                       ; preds = %if.then12, %if.else11
  %access.if.value13 = load i32, ptr %if.value9, align 4
  store i32 %access.if.value13, ptr %if.value, align 4
  br label %if.after3

if.else11:                                        ; preds = %if.then8
  store i32 0, ptr %if.value9, align 4
  br label %if.after10

if.then12:                                        ; preds = %if.then8
  store i32 1, ptr %if.value9, align 4
  br label %if.after10

if.after16:                                       ; preds = %if.then18, %if.else17
  %access.if.value19 = load i1, ptr %if.value15, align 1
  br i1 %access.if.value19, label %if.then23, label %if.else22

if.else17:                                        ; preds = %if.after3
  store i1 false, ptr %if.value15, align 1
  br label %if.after16

if.then18:                                        ; preds = %if.after3
  store i1 true, ptr %if.value15, align 1
  br label %if.after16

if.after21:                                       ; preds = %if.then23, %if.else22
  %access.if.value24 = load i32, ptr %if.value20, align 4
  br i1 true, label %if.then28, label %if.else27

if.else22:                                        ; preds = %if.after16
  store i32 2, ptr %if.value20, align 4
  br label %if.after21

if.then23:                                        ; preds = %if.after16
  store i32 1, ptr %if.value20, align 4
  br label %if.after21

if.after26:                                       ; preds = %if.then28, %if.else27
  %access.if.value29 = load i1, ptr %if.value25, align 1
  %and_op = and i1 %access.if.value29, true
  br i1 %and_op, label %if.then33, label %if.else32

if.else27:                                        ; preds = %if.after21
  store i1 false, ptr %if.value25, align 1
  br label %if.after26

if.then28:                                        ; preds = %if.after21
  store i1 true, ptr %if.value25, align 1
  br label %if.after26

if.after31:                                       ; preds = %if.then33, %if.else32
  %access.if.value34 = load i32, ptr %if.value30, align 4
  br i1 true, label %if.then38, label %if.else37

if.else32:                                        ; preds = %if.after26
  store i32 2, ptr %if.value30, align 4
  br label %if.after31

if.then33:                                        ; preds = %if.after26
  store i32 1, ptr %if.value30, align 4
  br label %if.after31

if.after36:                                       ; preds = %if.then38, %if.else37
  %access.if.value39 = load i1, ptr %if.value35, align 1
  br i1 %access.if.value39, label %if.then43, label %if.else42

if.else37:                                        ; preds = %if.after31
  store i1 false, ptr %if.value35, align 1
  br label %if.after36

if.then38:                                        ; preds = %if.after31
  store i1 true, ptr %if.value35, align 1
  br label %if.after36

if.after41:                                       ; preds = %if.after45, %if.else42
  %access.if.value49 = load i32, ptr %if.value40, align 4
  br i1 true, label %if.then53, label %if.else52

if.else42:                                        ; preds = %if.after36
  store i32 2, ptr %if.value40, align 4
  br label %if.after41

if.then43:                                        ; preds = %if.after36
  br i1 true, label %if.then47, label %if.else46

if.after45:                                       ; preds = %if.then47, %if.else46
  %access.if.value48 = load i32, ptr %if.value44, align 4
  store i32 %access.if.value48, ptr %if.value40, align 4
  br label %if.after41

if.else46:                                        ; preds = %if.then43
  store i32 2, ptr %if.value44, align 4
  br label %if.after45

if.then47:                                        ; preds = %if.then43
  store i32 1, ptr %if.value44, align 4
  br label %if.after45

if.after51:                                       ; preds = %if.then53, %if.else52
  %access.if.value54 = load i1, ptr %if.value50, align 1
  br i1 %access.if.value54, label %if.then63, label %if.else57

if.else52:                                        ; preds = %if.after41
  store i1 false, ptr %if.value50, align 1
  br label %if.after51

if.then53:                                        ; preds = %if.after41
  store i1 true, ptr %if.value50, align 1
  br label %if.after51

if.after56:                                       ; preds = %if.after65, %if.after59
  %access.if.value69 = load i32, ptr %if.value55, align 4
  ret void

if.else57:                                        ; preds = %if.after51
  br i1 true, label %if.then61, label %if.else60

if.after59:                                       ; preds = %if.then61, %if.else60
  %access.if.value62 = load i32, ptr %if.value58, align 4
  store i32 %access.if.value62, ptr %if.value55, align 4
  br label %if.after56

if.else60:                                        ; preds = %if.else57
  store i32 2, ptr %if.value58, align 4
  br label %if.after59

if.then61:                                        ; preds = %if.else57
  store i32 1, ptr %if.value58, align 4
  br label %if.after59

if.then63:                                        ; preds = %if.after51
  br i1 true, label %if.then67, label %if.else66

if.after65:                                       ; preds = %if.then67, %if.else66
  %access.if.value68 = load i32, ptr %if.value64, align 4
  store i32 %access.if.value68, ptr %if.value55, align 4
  br label %if.after56

if.else66:                                        ; preds = %if.then63
  store i32 2, ptr %if.value64, align 4
  br label %if.after65

if.then67:                                        ; preds = %if.then63
  store i32 1, ptr %if.value64, align 4
  br label %if.after65
}
