; ModuleID = 'tests.object_field_shorthand'
source_filename = "tests.object_field_shorthand"

define private void @tests_object_field_shorthand.tests() {
fn.entry:
  %object.alloca18 = alloca { i32 }, align 8
  %object.alloca17 = alloca { { i32 } }, align 8
  %object.alloca16 = alloca { { { i32 } } }, align 8
  %object.alloca9 = alloca { i32 }, align 8
  %object.alloca8 = alloca { { i32 } }, align 8
  %object.alloca7 = alloca { { { i32 } } }, align 8
  %object.alloca2 = alloca { i32 }, align 8
  %object.alloca1 = alloca { { i32 } }, align 8
  %object.alloca = alloca { i32 }, align 8
  %object.alloca.field.gep = getelementptr inbounds { i32 }, ptr %object.alloca, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep, align 4
  %access.object = load { i32 }, ptr %object.alloca, align 4
  call void @tests_object_field_shorthand.unbox({ i32 } %access.object)
  %object.alloca.field.gep3 = getelementptr inbounds { i32 }, ptr %object.alloca2, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep3, align 4
  %access.object4 = load { i32 }, ptr %object.alloca2, align 4
  %object.alloca.field.gep5 = getelementptr inbounds { { i32 } }, ptr %object.alloca1, i32 0, i32 0
  store { i32 } %access.object4, ptr %object.alloca.field.gep5, align 4
  %access.object6 = load { { i32 } }, ptr %object.alloca1, align 4
  call void @tests_object_field_shorthand.unbox.1({ { i32 } } %access.object6)
  %object.alloca.field.gep10 = getelementptr inbounds { i32 }, ptr %object.alloca9, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep10, align 4
  %access.object11 = load { i32 }, ptr %object.alloca9, align 4
  %object.alloca.field.gep12 = getelementptr inbounds { { i32 } }, ptr %object.alloca8, i32 0, i32 0
  store { i32 } %access.object11, ptr %object.alloca.field.gep12, align 4
  %access.object13 = load { { i32 } }, ptr %object.alloca8, align 4
  %object.alloca.field.gep14 = getelementptr inbounds { { { i32 } } }, ptr %object.alloca7, i32 0, i32 0
  store { { i32 } } %access.object13, ptr %object.alloca.field.gep14, align 4
  %access.object15 = load { { { i32 } } }, ptr %object.alloca7, align 4
  call void @tests_object_field_shorthand.unbox.2({ { { i32 } } } %access.object15)
  %object.alloca.field.gep19 = getelementptr inbounds { i32 }, ptr %object.alloca18, i32 0, i32 0
  store i32 2, ptr %object.alloca.field.gep19, align 4
  %access.object20 = load { i32 }, ptr %object.alloca18, align 4
  %object.alloca.field.gep21 = getelementptr inbounds { { i32 } }, ptr %object.alloca17, i32 0, i32 0
  store { i32 } %access.object20, ptr %object.alloca.field.gep21, align 4
  %access.object22 = load { { i32 } }, ptr %object.alloca17, align 4
  %object.alloca.field.gep23 = getelementptr inbounds { { { i32 } } }, ptr %object.alloca16, i32 0, i32 0
  store { { i32 } } %access.object22, ptr %object.alloca.field.gep23, align 4
  %access.object24 = load { { { i32 } } }, ptr %object.alloca16, align 4
  call void @tests_object_field_shorthand.unbox.3({ { { i32 } } } %access.object24)
  ret void
}

define private void @tests_object_field_shorthand.unbox({ i32 } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_object_field_shorthand.unbox.1({ { i32 } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_object_field_shorthand.unbox.2({ { { i32 } } } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_object_field_shorthand.unbox.3({ { { i32 } } } %parameter.x) {
fn.entry:
  ret void
}
