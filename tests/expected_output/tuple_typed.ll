; ModuleID = 'tuple_typed'
source_filename = "tuple_typed"

define private void @tests_tuple_typed.tests() {
fn.entry:
  %tuple.alloca1 = alloca { i32 }, align 8
  %tuple.alloca = alloca { i32 }, align 8
  %tuple.init.gep = getelementptr inbounds { i32 }, ptr %tuple.alloca, i32 0, i32 0
  store i32 0, ptr %tuple.init.gep, align 4
  %tuple.init.gep2 = getelementptr inbounds { i32 }, ptr %tuple.alloca1, i32 0, i32 0
  store i32 0, ptr %tuple.init.gep2, align 4
  %access.tuple.access = load { i32 }, ptr %tuple.alloca1, align 4
  call void @tests_tuple_typed.unbox({ i32 } %access.tuple.access)
  ret void
}

define private void @tests_tuple_typed.unbox({ i32 } %parameter.x) {
fn.entry:
  ret void
}
