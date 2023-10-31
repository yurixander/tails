; ModuleID = 'object'
source_filename = "object"

define private void @tests_object.tests() {
fn.entry:
  %object.alloca5 = alloca { i32, i32, i32 }, align 8
  %object.alloca1 = alloca { i32, i32 }, align 8
  %object.alloca = alloca { i32 }, align 8
  %object.alloca.field.gep = getelementptr inbounds { i32 }, ptr %object.alloca, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep, align 4
  %access.object = load { i32 }, ptr %object.alloca, align 4
  call void @tests_object.unbox({ i32 } %access.object)
  %object.alloca.field.gep2 = getelementptr inbounds { i32, i32 }, ptr %object.alloca1, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep2, align 4
  %object.alloca.field.gep3 = getelementptr inbounds { i32, i32 }, ptr %object.alloca1, i32 0, i32 1
  store i32 2, ptr %object.alloca.field.gep3, align 4
  %access.object4 = load { i32, i32 }, ptr %object.alloca1, align 4
  call void @tests_object.unbox.1({ i32, i32 } %access.object4)
  %object.alloca.field.gep6 = getelementptr inbounds { i32, i32, i32 }, ptr %object.alloca5, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep6, align 4
  %object.alloca.field.gep7 = getelementptr inbounds { i32, i32, i32 }, ptr %object.alloca5, i32 0, i32 1
  store i32 2, ptr %object.alloca.field.gep7, align 4
  %object.alloca.field.gep8 = getelementptr inbounds { i32, i32, i32 }, ptr %object.alloca5, i32 0, i32 2
  store i32 3, ptr %object.alloca.field.gep8, align 4
  %access.object9 = load { i32, i32, i32 }, ptr %object.alloca5, align 4
  call void @tests_object.unbox.2({ i32, i32, i32 } %access.object9)
  ret void
}

define private void @tests_object.unbox({ i32 } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_object.unbox.1({ i32, i32 } %parameter.x) {
fn.entry:
  ret void
}

define private void @tests_object.unbox.2({ i32, i32, i32 } %parameter.x) {
fn.entry:
  ret void
}
