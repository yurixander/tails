; ModuleID = 'tuple_single'
source_filename = "tuple_single"

@string_literal = private unnamed_addr constant [5 x i8] c"test\00", align 1

define private void @tests_tuple_single.tests() {
fn.entry:
  %tuple.alloca25 = alloca { i32 }, align 8
  %tuple.alloca24 = alloca { { i32 } }, align 8
  %tuple.alloca23 = alloca { { { i32 } } }, align 8
  %tuple.alloca18 = alloca { i32 }, align 8
  %tuple.alloca17 = alloca { { i32 } }, align 8
  %tuple.alloca14 = alloca { float }, align 8
  %tuple.alloca11 = alloca { i32 }, align 8
  %tuple.alloca8 = alloca { ptr }, align 8
  %tuple.alloca5 = alloca { ptr }, align 8
  %tuple.alloca1 = alloca { i32, i32 }, align 8
  %tuple.alloca = alloca { i32 }, align 8
  %tuple.init.gep = getelementptr inbounds { i32 }, ptr %tuple.alloca, i32 0, i32 0
  store i32 0, ptr %tuple.init.gep, align 4
  %access.tuple.access = load { i32 }, ptr %tuple.alloca, align 4
  call void @tests_tuple_single.unbox({ i32 } %access.tuple.access)
  %tuple.init.gep2 = getelementptr inbounds { i32, i32 }, ptr %tuple.alloca1, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep2, align 4
  %tuple.init.gep3 = getelementptr inbounds { i32, i32 }, ptr %tuple.alloca1, i32 0, i32 1
  store i32 2, ptr %tuple.init.gep3, align 4
  %access.tuple.access4 = load { i32, i32 }, ptr %tuple.alloca1, align 4
  call void @tests_tuple_single.unbox.1({ i32, i32 } %access.tuple.access4)
  %tuple.init.gep6 = getelementptr inbounds { ptr }, ptr %tuple.alloca5, i32 0, i32 0
  store ptr null, ptr %tuple.init.gep6, align 8
  %access.tuple.access7 = load { ptr }, ptr %tuple.alloca5, align 8
  call void @tests_tuple_single.unbox.2({ ptr } %access.tuple.access7)
  %tuple.init.gep9 = getelementptr inbounds { ptr }, ptr %tuple.alloca8, i32 0, i32 0
  store ptr @string_literal, ptr %tuple.init.gep9, align 8
  %access.tuple.access10 = load { ptr }, ptr %tuple.alloca8, align 8
  call void @tests_tuple_single.unbox.3({ ptr } %access.tuple.access10)
  %tuple.init.gep12 = getelementptr inbounds { i32 }, ptr %tuple.alloca11, i32 0, i32 0
  store i32 0, ptr %tuple.init.gep12, align 4
  %access.tuple.access13 = load { i32 }, ptr %tuple.alloca11, align 4
  call void @tests_tuple_single.unbox.4({ i32 } %access.tuple.access13)
  %tuple.init.gep15 = getelementptr inbounds { float }, ptr %tuple.alloca14, i32 0, i32 0
  store float 3.140000e+02, ptr %tuple.init.gep15, align 4
  %access.tuple.access16 = load { float }, ptr %tuple.alloca14, align 4
  call void @tests_tuple_single.unbox.5({ float } %access.tuple.access16)
  %tuple.init.gep19 = getelementptr inbounds { i32 }, ptr %tuple.alloca18, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep19, align 4
  %access.tuple.access20 = load { i32 }, ptr %tuple.alloca18, align 4
  %tuple.init.gep21 = getelementptr inbounds { { i32 } }, ptr %tuple.alloca17, i32 0, i32 0
  store { i32 } %access.tuple.access20, ptr %tuple.init.gep21, align 4
  %access.tuple.access22 = load { { i32 } }, ptr %tuple.alloca17, align 4
  call void @tests_tuple_single.unbox.6({ { i32 } } %access.tuple.access22)
  %tuple.init.gep26 = getelementptr inbounds { i32 }, ptr %tuple.alloca25, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep26, align 4
  %access.tuple.access27 = load { i32 }, ptr %tuple.alloca25, align 4
  %tuple.init.gep28 = getelementptr inbounds { { i32 } }, ptr %tuple.alloca24, i32 0, i32 0
  store { i32 } %access.tuple.access27, ptr %tuple.init.gep28, align 4
  %access.tuple.access29 = load { { i32 } }, ptr %tuple.alloca24, align 4
  %tuple.init.gep30 = getelementptr inbounds { { { i32 } } }, ptr %tuple.alloca23, i32 0, i32 0
  store { { i32 } } %access.tuple.access29, ptr %tuple.init.gep30, align 4
  %access.tuple.access31 = load { { { i32 } } }, ptr %tuple.alloca23, align 4
  call void @tests_tuple_single.unbox.7({ { { i32 } } } %access.tuple.access31)
  ret void
}

define private void @tests_tuple_single.unbox({ i32 } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_single.unbox.1({ i32, i32 } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_single.unbox.2({ ptr } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_single.unbox.3({ ptr } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_single.unbox.4({ i32 } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_single.unbox.5({ float } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_single.unbox.6({ { i32 } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_tuple_single.unbox.7({ { { i32 } } } %parameter.x) {
fn.entry:
  ret void
}
