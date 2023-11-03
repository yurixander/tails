; ModuleID = 'tests.tuple_nested'
source_filename = "tests.tuple_nested"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1

define private void @tests_tuple_nested.tests() {
fn.entry:
  %tuple.alloca46 = alloca { i32, i32, i32 }, align 8
  %tuple.alloca42 = alloca { i32, i32 }, align 8
  %tuple.alloca39 = alloca { i32 }, align 8
  %tuple.alloca38 = alloca { { i32 }, { i32, i32 }, { i32, i32, i32 } }, align 8
  %tuple.alloca31 = alloca { i32 }, align 8
  %tuple.alloca28 = alloca { i32 }, align 8
  %tuple.alloca25 = alloca { i32 }, align 8
  %tuple.alloca24 = alloca { { i32 }, { i32 }, { i32 } }, align 8
  %tuple.alloca18 = alloca { ptr }, align 8
  %tuple.alloca15 = alloca { i32 }, align 8
  %tuple.alloca14 = alloca { { i32 }, { ptr } }, align 8
  %tuple.alloca8 = alloca { ptr }, align 8
  %tuple.alloca5 = alloca { ptr }, align 8
  %tuple.alloca4 = alloca { { ptr }, { ptr } }, align 8
  %tuple.alloca1 = alloca { i32 }, align 8
  %tuple.alloca = alloca { { i32 } }, align 8
  %tuple.init.gep = getelementptr inbounds { i32 }, ptr %tuple.alloca1, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep, align 4
  %access.tuple.access = load { i32 }, ptr %tuple.alloca1, align 4
  %tuple.init.gep2 = getelementptr inbounds { { i32 } }, ptr %tuple.alloca, i32 0, i32 0
  store { i32 } %access.tuple.access, ptr %tuple.init.gep2, align 4
  %access.tuple.access3 = load { { i32 } }, ptr %tuple.alloca, align 4
  call void @tests_tuple_nested.a({ { i32 } } %access.tuple.access3)
  %tuple.init.gep6 = getelementptr inbounds { ptr }, ptr %tuple.alloca5, i32 0, i32 0
  store ptr null, ptr %tuple.init.gep6, align 8
  %access.tuple.access7 = load { ptr }, ptr %tuple.alloca5, align 8
  %tuple.init.gep9 = getelementptr inbounds { ptr }, ptr %tuple.alloca8, i32 0, i32 0
  store ptr null, ptr %tuple.init.gep9, align 8
  %access.tuple.access10 = load { ptr }, ptr %tuple.alloca8, align 8
  %tuple.init.gep11 = getelementptr inbounds { { ptr }, { ptr } }, ptr %tuple.alloca4, i32 0, i32 0
  store { ptr } %access.tuple.access7, ptr %tuple.init.gep11, align 8
  %tuple.init.gep12 = getelementptr inbounds { { ptr }, { ptr } }, ptr %tuple.alloca4, i32 0, i32 1
  store { ptr } %access.tuple.access10, ptr %tuple.init.gep12, align 8
  %access.tuple.access13 = load { { ptr }, { ptr } }, ptr %tuple.alloca4, align 8
  call void @tests_tuple_nested.a.1({ { ptr }, { ptr } } %access.tuple.access13)
  %tuple.init.gep16 = getelementptr inbounds { i32 }, ptr %tuple.alloca15, i32 0, i32 0
  store i32 123, ptr %tuple.init.gep16, align 4
  %access.tuple.access17 = load { i32 }, ptr %tuple.alloca15, align 4
  %tuple.init.gep19 = getelementptr inbounds { ptr }, ptr %tuple.alloca18, i32 0, i32 0
  store ptr @string_literal, ptr %tuple.init.gep19, align 8
  %access.tuple.access20 = load { ptr }, ptr %tuple.alloca18, align 8
  %tuple.init.gep21 = getelementptr inbounds { { i32 }, { ptr } }, ptr %tuple.alloca14, i32 0, i32 0
  store { i32 } %access.tuple.access17, ptr %tuple.init.gep21, align 4
  %tuple.init.gep22 = getelementptr inbounds { { i32 }, { ptr } }, ptr %tuple.alloca14, i32 0, i32 1
  store { ptr } %access.tuple.access20, ptr %tuple.init.gep22, align 8
  %access.tuple.access23 = load { { i32 }, { ptr } }, ptr %tuple.alloca14, align 8
  call void @tests_tuple_nested.a.2({ { i32 }, { ptr } } %access.tuple.access23)
  %tuple.init.gep26 = getelementptr inbounds { i32 }, ptr %tuple.alloca25, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep26, align 4
  %access.tuple.access27 = load { i32 }, ptr %tuple.alloca25, align 4
  %tuple.init.gep29 = getelementptr inbounds { i32 }, ptr %tuple.alloca28, i32 0, i32 0
  store i32 2, ptr %tuple.init.gep29, align 4
  %access.tuple.access30 = load { i32 }, ptr %tuple.alloca28, align 4
  %tuple.init.gep32 = getelementptr inbounds { i32 }, ptr %tuple.alloca31, i32 0, i32 0
  store i32 3, ptr %tuple.init.gep32, align 4
  %access.tuple.access33 = load { i32 }, ptr %tuple.alloca31, align 4
  %tuple.init.gep34 = getelementptr inbounds { { i32 }, { i32 }, { i32 } }, ptr %tuple.alloca24, i32 0, i32 0
  store { i32 } %access.tuple.access27, ptr %tuple.init.gep34, align 4
  %tuple.init.gep35 = getelementptr inbounds { { i32 }, { i32 }, { i32 } }, ptr %tuple.alloca24, i32 0, i32 1
  store { i32 } %access.tuple.access30, ptr %tuple.init.gep35, align 4
  %tuple.init.gep36 = getelementptr inbounds { { i32 }, { i32 }, { i32 } }, ptr %tuple.alloca24, i32 0, i32 2
  store { i32 } %access.tuple.access33, ptr %tuple.init.gep36, align 4
  %access.tuple.access37 = load { { i32 }, { i32 }, { i32 } }, ptr %tuple.alloca24, align 4
  call void @tests_tuple_nested.a.3({ { i32 }, { i32 }, { i32 } } %access.tuple.access37)
  %tuple.init.gep40 = getelementptr inbounds { i32 }, ptr %tuple.alloca39, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep40, align 4
  %access.tuple.access41 = load { i32 }, ptr %tuple.alloca39, align 4
  %tuple.init.gep43 = getelementptr inbounds { i32, i32 }, ptr %tuple.alloca42, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep43, align 4
  %tuple.init.gep44 = getelementptr inbounds { i32, i32 }, ptr %tuple.alloca42, i32 0, i32 1
  store i32 2, ptr %tuple.init.gep44, align 4
  %access.tuple.access45 = load { i32, i32 }, ptr %tuple.alloca42, align 4
  %tuple.init.gep47 = getelementptr inbounds { i32, i32, i32 }, ptr %tuple.alloca46, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep47, align 4
  %tuple.init.gep48 = getelementptr inbounds { i32, i32, i32 }, ptr %tuple.alloca46, i32 0, i32 1
  store i32 2, ptr %tuple.init.gep48, align 4
  %tuple.init.gep49 = getelementptr inbounds { i32, i32, i32 }, ptr %tuple.alloca46, i32 0, i32 2
  store i32 3, ptr %tuple.init.gep49, align 4
  %access.tuple.access50 = load { i32, i32, i32 }, ptr %tuple.alloca46, align 4
  %tuple.init.gep51 = getelementptr inbounds { { i32 }, { i32, i32 }, { i32, i32, i32 } }, ptr %tuple.alloca38, i32 0, i32 0
  store { i32 } %access.tuple.access41, ptr %tuple.init.gep51, align 4
  %tuple.init.gep52 = getelementptr inbounds { { i32 }, { i32, i32 }, { i32, i32, i32 } }, ptr %tuple.alloca38, i32 0, i32 1
  store { i32, i32 } %access.tuple.access45, ptr %tuple.init.gep52, align 4
  %tuple.init.gep53 = getelementptr inbounds { { i32 }, { i32, i32 }, { i32, i32, i32 } }, ptr %tuple.alloca38, i32 0, i32 2
  store { i32, i32, i32 } %access.tuple.access50, ptr %tuple.init.gep53, align 4
  %access.tuple.access54 = load { { i32 }, { i32, i32 }, { i32, i32, i32 } }, ptr %tuple.alloca38, align 4
  call void @tests_tuple_nested.a.4({ { i32 }, { i32, i32 }, { i32, i32, i32 } } %access.tuple.access54)
  ret void
}

define private void @tests_tuple_nested.a({ { i32 } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_nested.a.1({ { ptr }, { ptr } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_nested.a.2({ { i32 }, { ptr } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_nested.a.3({ { i32 }, { i32 }, { i32 } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_nested.a.4({ { i32 }, { i32, i32 }, { i32, i32, i32 } } %parameter.x) {
fn.entry:
  ret void
}
