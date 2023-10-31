; ModuleID = 'if_complex_condition'
source_filename = "if_complex_condition"

define private void @tests_if_complex_condition.tests() {
fn.entry:
  br i1 false, label %if.then, label %if.after

if.after:                                         ; preds = %fn.entry, %if.then
  br i1 true, label %if.then2, label %if.after1

if.then:                                          ; preds = %fn.entry
  br label %if.after

if.after1:                                        ; preds = %if.after, %if.then2
  br i1 true, label %if.then4, label %if.after3

if.then2:                                         ; preds = %if.after
  br label %if.after1

if.after3:                                        ; preds = %if.after1, %if.then4
  br i1 true, label %if.then6, label %if.after5

if.then4:                                         ; preds = %if.after1
  br label %if.after3

if.after5:                                        ; preds = %if.after3, %if.then6
  br i1 true, label %if.then8, label %if.after7

if.then6:                                         ; preds = %if.after3
  br label %if.after5

if.after7:                                        ; preds = %if.after5, %if.then8
  br i1 false, label %if.then10, label %if.after9

if.then8:                                         ; preds = %if.after5
  br label %if.after7

if.after9:                                        ; preds = %if.after7, %if.then10
  br i1 true, label %if.then12, label %if.after11

if.then10:                                        ; preds = %if.after7
  br label %if.after9

if.after11:                                       ; preds = %if.after9, %if.then12
  br i1 true, label %if.then14, label %if.after13

if.then12:                                        ; preds = %if.after9
  br label %if.after11

if.after13:                                       ; preds = %if.after11, %if.then14
  br i1 true, label %if.then16, label %if.after15

if.then14:                                        ; preds = %if.after11
  br label %if.after13

if.after15:                                       ; preds = %if.after13, %if.then16
  br i1 true, label %if.then18, label %if.after17

if.then16:                                        ; preds = %if.after13
  br label %if.after15

if.after17:                                       ; preds = %if.after15, %if.then18
  ret void

if.then18:                                        ; preds = %if.after15
  br label %if.after17
}
