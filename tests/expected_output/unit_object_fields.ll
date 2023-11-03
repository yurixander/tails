; ModuleID = 'tests.unit_object_fields'
source_filename = "tests.unit_object_fields"

define private void @tests_unit_object_fields.tests() {
fn.entry:
  %object.alloca1 = alloca { i32, ptr, i32 }, align 8
  %object.alloca = alloca { ptr }, align 8
  %object.alloca.field.gep = getelementptr inbounds { ptr }, ptr %object.alloca, i32 0, i32 0
  store ptr null, ptr %object.alloca.field.gep, align 8
  %object.alloca.field.gep2 = getelementptr inbounds { i32, ptr, i32 }, ptr %object.alloca1, i32 0, i32 0
  store i32 1, ptr %object.alloca.field.gep2, align 4
  %object.alloca.field.gep3 = getelementptr inbounds { i32, ptr, i32 }, ptr %object.alloca1, i32 0, i32 1
  store ptr null, ptr %object.alloca.field.gep3, align 8
  %object.alloca.field.gep4 = getelementptr inbounds { i32, ptr, i32 }, ptr %object.alloca1, i32 0, i32 2
  store i32 2, ptr %object.alloca.field.gep4, align 4
  ret void
}
