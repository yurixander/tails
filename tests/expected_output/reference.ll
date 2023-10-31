; ModuleID = 'reference'
source_filename = "reference"

define private void @tests_reference.tests() {
fn.entry:
  %reference.literal.alloca3 = alloca i32, align 4
  %binding.reference.alloca2 = alloca ptr, align 8
  %reference.literal.alloca1 = alloca i32, align 4
  %binding.reference.alloca = alloca ptr, align 8
  %reference.literal.alloca = alloca i32, align 4
  store i32 123, ptr %reference.literal.alloca, align 4
  store ptr %reference.literal.alloca, ptr %binding.reference.alloca, align 8
  store i32 123, ptr %reference.literal.alloca1, align 4
  store ptr %reference.literal.alloca1, ptr %binding.reference.alloca2, align 8
  call void @tests_reference.receive(ptr %binding.reference.alloca2)
  store i32 321, ptr %reference.literal.alloca3, align 4
  call void @tests_reference.receive(ptr %reference.literal.alloca3)
  ret void
}

define private void @tests_reference.receive(ptr %parameter.a) {
fn.entry:
  ret void
}
