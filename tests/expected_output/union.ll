; ModuleID = 'tests.union'
source_filename = "tests.union"

define private void @tests_union.tests() {
fn.entry:
  %object.alloca = alloca { i32 }, align 8
  %union.instance.alloca11 = alloca { i64, [4 x i32] }, align 8
  %tuple.alloca7 = alloca { i32 }, align 8
  %union.instance.alloca5 = alloca { i64, [4 x i32] }, align 8
  %tuple.alloca = alloca { i32, i32 }, align 8
  %union.instance.alloca1 = alloca { i64, [4 x i32] }, align 8
  %union.instance.alloca = alloca { i64, [4 x i32] }, align 8
  %union.instance.tag.gep = getelementptr inbounds { i64, [4 x i32] }, ptr %union.instance.alloca, i32 0, i32 0
  store i64 1, ptr %union.instance.tag.gep, align 4
  %union.instance.value.gep = getelementptr inbounds { i64, [4 x i32] }, ptr %union.instance.alloca, i32 0, i32 1
  store i32 1, ptr %union.instance.value.gep, align 4
  %union.instance.tag.gep2 = getelementptr inbounds { i64, [4 x i32] }, ptr %union.instance.alloca1, i32 0, i32 0
  store i64 2, ptr %union.instance.tag.gep2, align 4
  %tuple.init.gep = getelementptr inbounds { i32, i32 }, ptr %tuple.alloca, i32 0, i32 0
  store i32 2, ptr %tuple.init.gep, align 4
  %tuple.init.gep3 = getelementptr inbounds { i32, i32 }, ptr %tuple.alloca, i32 0, i32 1
  store i32 0, ptr %tuple.init.gep3, align 4
  %access.tuple.access = load { i32, i32 }, ptr %tuple.alloca, align 4
  %union.instance.value.gep4 = getelementptr inbounds { i64, [4 x i32] }, ptr %union.instance.alloca1, i32 0, i32 1
  store { i32, i32 } %access.tuple.access, ptr %union.instance.value.gep4, align 4
  %union.instance.tag.gep6 = getelementptr inbounds { i64, [4 x i32] }, ptr %union.instance.alloca5, i32 0, i32 0
  store i64 3, ptr %union.instance.tag.gep6, align 4
  %tuple.init.gep8 = getelementptr inbounds { i32 }, ptr %tuple.alloca7, i32 0, i32 0
  store i32 1, ptr %tuple.init.gep8, align 4
  %access.tuple.access9 = load { i32 }, ptr %tuple.alloca7, align 4
  %union.instance.value.gep10 = getelementptr inbounds { i64, [4 x i32] }, ptr %union.instance.alloca5, i32 0, i32 1
  store { i32 } %access.tuple.access9, ptr %union.instance.value.gep10, align 4
  %union.instance.tag.gep12 = getelementptr inbounds { i64, [4 x i32] }, ptr %union.instance.alloca11, i32 0, i32 0
  store i64 4, ptr %union.instance.tag.gep12, align 4
  %object.alloca.field.gep = getelementptr inbounds { i32 }, ptr %object.alloca, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep, align 4
  %access.object = load { i32 }, ptr %object.alloca, align 4
  %union.instance.value.gep13 = getelementptr inbounds { i64, [4 x i32] }, ptr %union.instance.alloca11, i32 0, i32 1
  store { i32 } %access.object, ptr %union.instance.value.gep13, align 4
  %access.union.instance = load { i64, [4 x i32] }, ptr %union.instance.alloca11, align 4
  ret void
}
