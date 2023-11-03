; ModuleID = 'tests.if_values'
source_filename = "tests.if_values"

@string_literal = private unnamed_addr constant [2 x i8] c"b\00", align 1
@string_literal.1 = private unnamed_addr constant [2 x i8] c"a\00", align 1
@guard.message = private unnamed_addr constant [43 x i8] c"runtime assertion failed: division by zero\00", align 1
@guard.note.message = private unnamed_addr constant [131 x i8] c"the program was aborted by a compiler guard to ensure that it does not enter an invalid state which may lead to undefined behavior\00", align 1

define private void @tests_if_values.tests() {
fn.entry:
  %if.value38 = alloca i32, align 4
  %if.value33 = alloca i32, align 4
  %if.value27 = alloca i32, align 4
  %if.value22 = alloca i32, align 4
  %reference.literal.alloca20 = alloca i32, align 4
  %reference.literal.alloca = alloca i32, align 4
  %if.value16 = alloca ptr, align 8
  %if.value11 = alloca i32, align 4
  %if.value6 = alloca ptr, align 8
  %if.value1 = alloca float, align 4
  %if.value = alloca i32, align 4
  br i1 true, label %if.then, label %if.else

if.after:                                         ; preds = %if.then, %if.else
  %access.if.value = load i32, ptr %if.value, align 4
  br i1 true, label %if.then4, label %if.else3

if.else:                                          ; preds = %fn.entry
  store i32 0, ptr %if.value, align 4
  br label %if.after

if.then:                                          ; preds = %fn.entry
  store i32 1, ptr %if.value, align 4
  br label %if.after

if.after2:                                        ; preds = %if.then4, %if.else3
  %access.if.value5 = load float, ptr %if.value1, align 4
  br i1 true, label %if.then9, label %if.else8

if.else3:                                         ; preds = %if.after
  store float 1.618000e+03, ptr %if.value1, align 4
  br label %if.after2

if.then4:                                         ; preds = %if.after
  store float 3.140000e+02, ptr %if.value1, align 4
  br label %if.after2

if.after7:                                        ; preds = %if.then9, %if.else8
  %access.if.value10 = load ptr, ptr %if.value6, align 8
  br i1 true, label %if.then14, label %if.else13

if.else8:                                         ; preds = %if.after2
  store ptr @string_literal, ptr %if.value6, align 8
  br label %if.after7

if.then9:                                         ; preds = %if.after2
  store ptr @string_literal.1, ptr %if.value6, align 8
  br label %if.after7

if.after12:                                       ; preds = %if.then14, %if.else13
  %access.if.value15 = load i32, ptr %if.value11, align 4
  br i1 true, label %if.then19, label %if.else18

if.else13:                                        ; preds = %if.after7
  store i32 -2, ptr %if.value11, align 4
  br label %if.after12

if.then14:                                        ; preds = %if.after7
  store i32 -1, ptr %if.value11, align 4
  br label %if.after12

if.after17:                                       ; preds = %if.then19, %if.else18
  %access.if.value21 = load ptr, ptr %if.value16, align 8
  br i1 true, label %if.then25, label %if.else24

if.else18:                                        ; preds = %if.after12
  store i32 2, ptr %reference.literal.alloca, align 4
  store ptr %reference.literal.alloca, ptr %if.value16, align 8
  br label %if.after17

if.then19:                                        ; preds = %if.after12
  store i32 1, ptr %reference.literal.alloca20, align 4
  store ptr %reference.literal.alloca20, ptr %if.value16, align 8
  br label %if.after17

if.after23:                                       ; preds = %if.then25, %if.else24
  %access.if.value26 = load i32, ptr %if.value22, align 4
  br i1 true, label %if.then30, label %if.else29

if.else24:                                        ; preds = %if.after17
  store i32 0, ptr %if.value22, align 4
  br label %if.after23

if.then25:                                        ; preds = %if.after17
  store i32 2, ptr %if.value22, align 4
  br label %if.after23

if.after28:                                       ; preds = %guard.continuation31, %guard.continuation
  %access.if.value32 = load i32, ptr %if.value27, align 4
  br i1 true, label %if.then36, label %if.else35

if.else29:                                        ; preds = %if.after23
  br i1 true, label %guard.continuation, label %guard.failure

guard.continuation:                               ; preds = %if.else29
  store i32 1, ptr %if.value27, align 4
  br label %if.after28

guard.failure:                                    ; preds = %if.then42, %if.else40, %if.then30, %if.else29
  %guard.puts = call i32 @puts(ptr @guard.message)
  %guard.note.puts = call i32 @puts(ptr @guard.note.message)
  call void @abort()
  unreachable

if.then30:                                        ; preds = %if.after23
  br i1 true, label %guard.continuation31, label %guard.failure

guard.continuation31:                             ; preds = %if.then30
  store i32 1, ptr %if.value27, align 4
  br label %if.after28

if.after34:                                       ; preds = %if.then36, %if.else35
  %access.if.value37 = load i32, ptr %if.value33, align 4
  br i1 true, label %if.then42, label %if.else40

if.else35:                                        ; preds = %if.after28
  store i32 0, ptr %if.value33, align 4
  br label %if.after34

if.then36:                                        ; preds = %if.after28
  store i32 2, ptr %if.value33, align 4
  br label %if.after34

if.after39:                                       ; preds = %guard.continuation43, %guard.continuation41
  %access.if.value44 = load i32, ptr %if.value38, align 4
  ret void

if.else40:                                        ; preds = %if.after34
  br i1 true, label %guard.continuation41, label %guard.failure

guard.continuation41:                             ; preds = %if.else40
  store i32 1, ptr %if.value38, align 4
  br label %if.after39

if.then42:                                        ; preds = %if.after34
  br i1 true, label %guard.continuation43, label %guard.failure

guard.continuation43:                             ; preds = %if.then42
  store i32 1, ptr %if.value38, align 4
  br label %if.after39
}

declare i32 @puts(ptr)

; Function Attrs: noreturn
declare void @abort() #0

attributes #0 = { noreturn }
