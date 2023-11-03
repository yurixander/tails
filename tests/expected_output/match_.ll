; ModuleID = 'tests.match_'
source_filename = "tests.match_"

define private void @tests_match_.tests() {
fn.entry:
  %match.value10 = alloca i32, align 4
  %match.value2 = alloca i32, align 4
  %match.value = alloca i32, align 4
  br label %match.case

match.after:                                      ; preds = %match.then
  br label %match.case3

match.case:                                       ; preds = %match.case, %fn.entry
  br i1 true, label %match.then, label %match.case

match.then:                                       ; preds = %match.case
  store i32 2, ptr %match.value, align 4
  br label %match.after

match.after1:                                     ; preds = %match.then8, %match.then7, %match.then6
  br label %match.case11

match.case3:                                      ; preds = %match.case3, %match.after
  br i1 false, label %match.then6, label %match.case3

match.case4:                                      ; preds = %match.case4
  br i1 false, label %match.then7, label %match.case4

match.case5:                                      ; preds = %match.case5
  br i1 false, label %match.then8, label %match.case5

match.then6:                                      ; preds = %match.case3
  store i32 2, ptr %match.value2, align 4
  br label %match.after1

match.then7:                                      ; preds = %match.case4
  store i32 3, ptr %match.value2, align 4
  br label %match.after1

match.then8:                                      ; preds = %match.case5
  store i32 4, ptr %match.value2, align 4
  br label %match.after1

match.after9:                                     ; preds = %match.then14, %match.then13
  ret void

match.case11:                                     ; preds = %match.case11, %match.after1
  br i1 false, label %match.then13, label %match.case11

match.case12:                                     ; preds = %match.case12
  br i1 false, label %match.then14, label %match.case12

match.then13:                                     ; preds = %match.case11
  store i32 1, ptr %match.value10, align 4
  br label %match.after9

match.then14:                                     ; preds = %match.case12
  store i32 2, ptr %match.value10, align 4
  br label %match.after9
}
