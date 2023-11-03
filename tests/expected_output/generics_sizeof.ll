; ModuleID = 'tests.generics_sizeof'
source_filename = "tests.generics_sizeof"

define private void @tests_generics_sizeof.tests() {
fn.entry:
  call void @tests_generics_sizeof.a(i64 ptrtoint (ptr getelementptr (i32, ptr null, i32 1) to i64))
  %call = call i64 @tests_generics_sizeof.b()
  %call.monomorphism.memo = call i64 @tests_generics_sizeof.b()
  %call.monomorphism.memo1 = call i64 @tests_generics_sizeof.b()
  %call.monomorphism.memo2 = call i64 @tests_generics_sizeof.b()
  ret void
}

define private void @tests_generics_sizeof.a(i64 %parameter.x) {
fn.entry:
  ret void
}

define private i64 @tests_generics_sizeof.b() {
fn.entry:
  ret i64 ptrtoint (ptr getelementptr (i32, ptr null, i32 1) to i64)
}
