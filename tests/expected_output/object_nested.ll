; ModuleID = 'tests.object_nested'
source_filename = "tests.object_nested"

define private void @tests_object_nested.tests() {
fn.entry:
  %object.alloca18 = alloca { i32 }, align 8
  %object.alloca14 = alloca { i32 }, align 8
  %object.alloca13 = alloca { { i32 }, { i32 } }, align 8
  %object.alloca6 = alloca { i32 }, align 8
  %object.alloca5 = alloca { { i32 } }, align 8
  %object.alloca4 = alloca { { { i32 } } }, align 8
  %object.alloca1 = alloca { i32 }, align 8
  %object.alloca = alloca { { i32 } }, align 8
  %object.alloca.field.gep = getelementptr inbounds { i32 }, ptr %object.alloca1, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep, align 4
  %access.object = load { i32 }, ptr %object.alloca1, align 4
  %object.alloca.field.gep2 = getelementptr inbounds { { i32 } }, ptr %object.alloca, i32 0, i32 0
  store { i32 } %access.object, ptr %object.alloca.field.gep2, align 4
  %access.object3 = load { { i32 } }, ptr %object.alloca, align 4
  call void @tests_object_nested.unbox({ { i32 } } %access.object3)
  %object.alloca.field.gep7 = getelementptr inbounds { i32 }, ptr %object.alloca6, i32 0, i32 0
  store i32 2, ptr %object.alloca.field.gep7, align 4
  %access.object8 = load { i32 }, ptr %object.alloca6, align 4
  %object.alloca.field.gep9 = getelementptr inbounds { { i32 } }, ptr %object.alloca5, i32 0, i32 0
  store { i32 } %access.object8, ptr %object.alloca.field.gep9, align 4
  %access.object10 = load { { i32 } }, ptr %object.alloca5, align 4
  %object.alloca.field.gep11 = getelementptr inbounds { { { i32 } } }, ptr %object.alloca4, i32 0, i32 0
  store { { i32 } } %access.object10, ptr %object.alloca.field.gep11, align 4
  %access.object12 = load { { { i32 } } }, ptr %object.alloca4, align 4
  call void @tests_object_nested.unbox.1({ { { i32 } } } %access.object12)
  %object.alloca.field.gep15 = getelementptr inbounds { i32 }, ptr %object.alloca14, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep15, align 4
  %access.object16 = load { i32 }, ptr %object.alloca14, align 4
  %object.alloca.field.gep17 = getelementptr inbounds { { i32 }, { i32 } }, ptr %object.alloca13, i32 0, i32 0
  store { i32 } %access.object16, ptr %object.alloca.field.gep17, align 4
  %object.alloca.field.gep19 = getelementptr inbounds { i32 }, ptr %object.alloca18, i32 0, i32 0
  store i32 2, ptr %object.alloca.field.gep19, align 4
  %access.object20 = load { i32 }, ptr %object.alloca18, align 4
  %object.alloca.field.gep21 = getelementptr inbounds { { i32 }, { i32 } }, ptr %object.alloca13, i32 0, i32 1
  store { i32 } %access.object20, ptr %object.alloca.field.gep21, align 4
  %access.object22 = load { { i32 }, { i32 } }, ptr %object.alloca13, align 4
  call void @tests_object_nested.unbox.2({ { i32 }, { i32 } } %access.object22)
  ret void
}

define private void @tests_object_nested.unbox({ { i32 } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_object_nested.unbox.1({ { { i32 } } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_object_nested.unbox.2({ { i32 }, { i32 } } %parameter.x) {
fn.entry:
  ret void
}
