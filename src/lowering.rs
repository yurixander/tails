//! The lowering phase transforms the AST into corresponding LLVM IR constructs
//! by making use of the `inkwell` crate, which is an LLVM bindings wrapper library
//! crate for Rust.
//!
//! The resulting LLVM IR module can be emitted as a string, which can then be compiled
//! by an external tool, such as `llc` or `clang`, or a driver.
//!
//! This phase assumes that the AST has been fully type-checked, and that the symbol table
//! has been populated with all necessary information to perform lookups during lowering.
//!
//! In the case that any assumptions are violated, diagnostics will not be emitted, and
//! instead, a panic will occur.

use inkwell::{
  types::BasicType,
  values::{AnyValue, BasicValue},
};

use crate::{
  assert_extract, ast, auxiliary,
  lowering_ctx::{self, BUG_LLVM_VALUE, BUG_TYPE_NEVER_UNIT},
  resolution, symbol_table, types, visit,
};

pub(crate) const ENTRY_POINT_NAME: &str = "main";

const BUG_CLOSURE_ENV_PARAM: &str = "LLVM function buffer for current closure should contain at least one parameter for the closure captures environment";

pub(crate) const BUG_BUILDER_UNSET: &str =
  "LLVM builder should be positioned within a block when building instructions";

impl<'a, 'llvm> visit::Visitor<Option<inkwell::values::BasicValueEnum<'llvm>>>
  for lowering_ctx::LoweringContext<'a, 'llvm>
{
  fn default_value(&mut self) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    None
  }

  fn enter_item(&mut self, item: &ast::Item) {
    // Sanity check.
    if let Some(registry_id) = item.find_registry_id() {
      assert!(self.symbol_table.registry.contains_key(registry_id));
    }
  }

  fn visit_with(&mut self, with: &ast::With) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // TODO: Implement lowering logic.
    todo!();
  }

  fn visit_try(&mut self, _item: &ast::Try) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // TODO: Implement. This would make use of the `invoke` instruction?
    todo!()
  }

  fn visit_union_instance(
    &mut self,
    union_instance: &ast::UnionInstance,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // CONSIDER: Breaking function apart, since it's getting too long and thus complex.

    let union_variant = assert_extract!(
      self
        .symbol_table
        .follow_link(&union_instance.path.link_id)
        .expect(auxiliary::BUG_NAME_RESOLUTION),
      symbol_table::RegistryItem::UnionVariant
    );

    let union_item = self
      .symbol_table
      .registry
      .get(&union_variant.union_id)
      .expect(auxiliary::BUG_NAME_RESOLUTION);

    let union = crate::assert_extract!(union_item, symbol_table::RegistryItem::Union);

    // Lower types early on to avoid creating LLVM instructions if
    // any required type is unit, and thus would not evaluate to anything.
    let llvm_union_type = self.lower_type(&types::Type::Union(std::rc::Rc::clone(&union)))?;
    let llvm_variant_type = self.lower_union_variant_type(&union_variant.kind)?;

    let llvm_base_alloca = self.alloca(llvm_union_type, "union.instance.alloca");
    let llvm_context = self.llvm_module.get_context();

    // Create a tag GEP to be filled in.
    // OPTIMIZE: Can be optimized by having their tag ids be indexes. Then the type would be i32 (or less). Or can just be left like this? Type might still need to be adjusted.
    let llvm_tag_id = llvm_context
      .i64_type()
      // SAFETY: Unchecked conversion.
      // REVIEW: Why is the id being used as the value? Shouldn't it be the index? Or does this make it globally unique? Whatever it is, document it since it's not obvious.
      .const_int(union_variant.registry_id.0 as u64, false);

    let llvm_tag_gep = self
      .llvm_builder
      .build_struct_gep(
        llvm_union_type,
        llvm_base_alloca,
        0,
        "union.instance.tag.gep",
      )
      // REVISE: Use `expect` instead.
      .unwrap();

    // Fill-in the tag.
    self
      .llvm_builder
      .build_store(llvm_tag_gep, llvm_tag_id)
      .expect(BUG_BUILDER_UNSET);

    // OPTIMIZE: Can be optimized by having their tag ids be indexes. Then the type would be i32 (or less). Or can just be left like this? Type might still need to be adjusted.
    let llvm_tag_type = llvm_context.i64_type();

    let llvm_variant_type_wrapper = llvm_context
      .struct_type(&[llvm_tag_type.into(), llvm_variant_type.into()], false)
      .ptr_type(inkwell::AddressSpace::default());

    // Bitcast the base sum type to our specific variant. This specializes the sum
    // type.
    let llvm_variant_instance_wrapper = self
      .llvm_builder
      .build_bitcast(
        llvm_base_alloca,
        llvm_variant_type_wrapper,
        "union.instance.bitcast",
      )
      .expect(BUG_BUILDER_UNSET);

    // Lower the value of the sum type instance, to be filled-in.
    let llvm_instance_value_opt = match &union_instance.value {
      ast::UnionInstanceValue::Value(value) => self
        // REVIEW: Should access mode be set here? Or just use `accept`? Explain why.
        .lower_with_access_mode(&value, lowering_ctx::AccessMode::Value)
        .expect("instance value's type was checked to not be unit previously"),
      // TODO: Implement singleton case. Should simply be an enumerated integer list, depending on the position of the original union declaration. Mixing singletons and other values is not allowed by the semantic checker.
      ast::UnionInstanceValue::Singleton(_) => llvm_context
        .i64_type()
        // TODO: Need to determine in which index (relative to other singletons) does this singleton occur in the target union of this instance. Might be a good idea to keep track of the singleton's indexes as part of their respective enum variants fields. In other words, provided during parsing.
        .const_int(0, false)
        .as_basic_value_enum(),
      ast::UnionInstanceValue::String(string) => {
        self.intern_or_get_string(string, "union.instance.string")
      }
    };

    // Fill in the union variants's value. No need to fill in the tag value; it
    // has been already previously filled before the bitcast.
    let llvm_variant_instance_value_gep = self
      .llvm_builder
      .build_struct_gep(
        llvm_union_type,
        llvm_variant_instance_wrapper.into_pointer_value(),
        1,
        "union.instance.value.gep",
      )
      // REVISE: Use `expect` instead.
      .unwrap();

    self
      .llvm_builder
      .build_store(llvm_variant_instance_value_gep, llvm_instance_value_opt)
      .expect(BUG_BUILDER_UNSET);

    // REVIEW: Continue implementation? Anything missing?

    Some(self.access_if_mode_applies(llvm_union_type, llvm_base_alloca, "union.instance"))
  }

  fn visit_tuple_indexing(
    &mut self,
    tuple_indexing: &ast::TupleIndex,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // Lower types early on to avoid creating LLVM instructions if
    // the resulting value won't evaluate regardless.
    let llvm_field_type = self.lower_type_by_id(&tuple_indexing.type_id)?;

    // TODO: Assert `tuple_indexing.indexed_tuple` is an LLVM struct type.

    let llvm_struct_ptr = self
      // Do not access the tuple, since the following code expects a pointer
      // to a struct alloca, and there are no constant structs.
      .lower_with_access_mode(
        &tuple_indexing.indexed_tuple,
        lowering_ctx::AccessMode::None,
      )
      .expect(lowering_ctx::BUG_LLVM_VALUE)
      .into_pointer_value();

    let llvm_field_gep = self
      .llvm_builder
      .build_struct_gep(
        llvm_field_type,
        llvm_struct_ptr,
        tuple_indexing.index,
        "tuple.field.gep",
      )
      // REVISE: Use `expect` instead. If the reason is repeated among other similar instances, create a clause or reason global constant.
      .unwrap();

    // NOTE: Since object members are unknown, we must be careful when we apply
    // access rules (need to consider cases where exceptions need to be made,
    // ex. string literals). However, in this specific case, the layer that will
    // be peeled off is not that of those exceptions, instead it is only that of the
    // LLVM GEP instruction.
    Some(self.access_if_mode_applies(llvm_field_type, llvm_field_gep, "tuple.field.access"))
  }

  fn visit_union(&mut self, union: &ast::Union) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // TODO: If the sum type isn't in C-styled enums, return here.
    let llvm_context = self.llvm_module.get_context();

    for (index, variant) in union.variants.iter().enumerate() {
      let llvm_name = self.mangle_name(&format!("enum.{}.{}", union.name, variant.0));

      let llvm_variant_global = self.llvm_module.add_global(
        llvm_context.i32_type(),
        Some(inkwell::AddressSpace::from(Self::LLVM_CONST_ADDRESS_SPACE)),
        &llvm_name,
      );

      llvm_variant_global.set_initializer(
        &llvm_context
          .i32_type()
          .const_int(Self::assert_trunc_cast(index), false),
      );
    }

    None
  }

  fn visit_tuple(&mut self, tuple: &ast::Tuple) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // If the tuple's type is unit, do not proceed to allocate it.
    let llvm_struct_type = self.lower_type_by_id(&tuple.type_id)?.into_struct_type();

    let llvm_struct_alloca = self.alloca(llvm_struct_type, "tuple.alloca");

    // OPTIMIZE: Collecting into an intermediate vector is not necessary? Since this is used only once as an iterator.
    let llvm_tuple_values = tuple
      .elements
      .iter()
      .map(|element| {
        self
          // All tuple elements are used as values, to be stored in the allocated
          // LLVM struct representing the tuple. Thus, they should be accessed.
          .lower_with_access_mode(element, lowering_ctx::AccessMode::Value)
          .expect(lowering_ctx::BUG_LLVM_VALUE)
      })
      .collect::<Vec<_>>();

    for (index, value) in llvm_tuple_values.into_iter().enumerate() {
      let llvm_field_gep = self
        .llvm_builder
        .build_struct_gep(
          llvm_struct_type,
          llvm_struct_alloca,
          Self::assert_trunc_cast(index),
          "tuple.init.gep",
        )
        // SAFETY: Undocumented unwrap.
        .unwrap();

      self
        .llvm_builder
        .build_store(llvm_field_gep, value)
        .expect(BUG_BUILDER_UNSET);
    }

    Some(self.access_if_mode_applies(
      llvm_struct_type.as_basic_type_enum(),
      llvm_struct_alloca,
      "tuple.access",
    ))
  }

  fn visit_match(&mut self, match_: &ast::Match) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_subject = self
      // If applicable, access the subject since it will be dealt with as
      // value.
      .lower_with_access_mode(&match_.subject, lowering_ctx::AccessMode::Value)
      .expect(lowering_ctx::BUG_LLVM_VALUE);

    // TODO: Awaiting implementation of comparison of more than just integers and floats. Note that characters are considered integers in LLVM IR.
    assert!(
      llvm_subject.is_int_value() || llvm_subject.is_float_value(),
      "only integer and float values are supported for now"
    );

    let llvm_function_buffer = self
      .llvm_function_buffer
      .expect(auxiliary::BUG_BUFFER_CONTRACT);

    let llvm_context = self.llvm_module.get_context();
    let llvm_after_block = llvm_context.append_basic_block(llvm_function_buffer, "match.after");
    let llvm_type = self.lower_type_by_id(&match_.type_id);
    let llvm_value_alloca = llvm_type.map(|llvm_type| self.alloca(llvm_type, "match.value"));

    let llvm_bridge_blocks = match_
      .arms
      .iter()
      .map(|_| llvm_context.append_basic_block(llvm_function_buffer, "match.case"))
      .collect::<Vec<_>>();

    let first_case = llvm_bridge_blocks
      .first()
      // NOTE: LLVM value cloning is cheap.
      .cloned()
      .expect("there should be at least one match case");

    self
      .llvm_builder
      .build_unconditional_branch(first_case)
      .expect(BUG_BUILDER_UNSET);

    // BUG: Edge case with default case being first.

    for (index, case) in match_.arms.iter().enumerate() {
      self.llvm_builder.position_at_end(llvm_bridge_blocks[index]);

      let llvm_case_comparison = self
        .lower_with_access_mode(&case.case, lowering_ctx::AccessMode::Value)
        .expect(lowering_ctx::BUG_LLVM_VALUE);

      let llvm_case = self.build_match_comparison(
        llvm_subject.is_int_value(),
        llvm_subject,
        llvm_case_comparison,
      );

      let llvm_comparison = self.build_match_comparison(
        llvm_subject.is_int_value(),
        llvm_subject,
        llvm_case.as_basic_value_enum(),
      );

      let llvm_then_block = llvm_context.append_basic_block(llvm_function_buffer, "match.then");

      self
        .llvm_builder
        .build_conditional_branch(
          llvm_comparison,
          llvm_then_block,
          llvm_bridge_blocks
            .get(index)
            .unwrap_or(&llvm_after_block)
            // NOTE: The LLVM basic block's pointer is copied, not the block itself.
            .to_owned(),
        )
        .expect(BUG_BUILDER_UNSET);

      self.llvm_builder.position_at_end(llvm_then_block);

      let llvm_then_value = self
        .visit_expr(&case.body)
        .expect(lowering_ctx::BUG_LLVM_VALUE);

      // If the comparison holds true, assign the final value of this match
      // expression to the value of the this case branch.
      if let Some(llvm_value_alloca) = llvm_value_alloca {
        self
          .llvm_builder
          .build_store(llvm_value_alloca, llvm_then_value)
          .expect(BUG_BUILDER_UNSET);
      }

      self
        .llvm_builder
        .build_unconditional_branch(llvm_after_block)
        .expect(BUG_BUILDER_UNSET);
    }

    // TODO: Missing logic for the default case.

    self.llvm_builder.position_at_end(llvm_after_block);

    // REVISE: Prefer a built-in method alternative instead of using `join_two`. Then, delete the `join_two` utility function.
    auxiliary::join_two_opts(&llvm_type, &llvm_value_alloca).map(
      |(llvm_type, llvm_value_alloca)| {
        // NOTE: LLVM value clones are cheap.
        self.access_if_mode_applies(
          llvm_type.to_owned(),
          llvm_value_alloca.to_owned(),
          "match.value",
        )
      },
    )
  }

  fn visit_statement(
    &mut self,
    statement: &ast::Statement,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // REVIEW: The `if` construct unwraps statements when lowering `llvm_else_branch_value` for some reason (expecting it to yield an LLVM value when lowering?).
    match statement {
      ast::Statement::InlineExpr(inline_expr) => self.visit_expr(inline_expr),
      ast::Statement::Binding(binding) => self.visit_item(&ast::Item::Binding(binding.clone())),
      ast::Statement::Constant(constant) => self.visit_item(&ast::Item::Constant(constant.clone())),
    };

    None
  }

  fn visit_discard(
    &mut self,
    discard: &ast::Discard,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    self.visit_expr(&discard.0);

    None
  }

  fn visit_sizeof(
    &mut self,
    sizeof: &ast::Sizeof,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // Unit types have a size of zero. If they are pointers, or any other
    // wrapper types, they are no longer considered unit types. This is
    // because even pointer types have a size.
    Some(match self.lower_type(&sizeof.ty) {
      // REVISE: Use `expect` instead, and specify reasoning.
      Some(ty) => ty.size_of().unwrap().as_basic_value_enum(),
      // If the type is unit, then the size is zero.
      None => self
        .llvm_module
        .get_context()
        .i32_type()
        .const_zero()
        .as_basic_value_enum(),
    })
  }

  fn visit_group(&mut self, group: &ast::Group) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    self.visit_expr(&group.0)
  }

  fn visit_object_access(
    &mut self,
    object_access: &ast::ObjectAccess,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_struct = self
      // TODO: Explain why nothing is being accessed here.
      .lower_with_access_mode(&object_access.object, lowering_ctx::AccessMode::None)
      .expect(lowering_ctx::BUG_LLVM_VALUE);

    const BUG_FIELD_MISSING: &str = "field should exist on the base object by the lowering phase";

    let base_expr_type = self.resolve_type_by_id(&object_access.base_expr_type_id);

    let object_type = match base_expr_type.as_ref() {
      types::Type::Object(object_type) => object_type,
      _ => unreachable!("{}", BUG_FIELD_MISSING),
    };

    // TODO: Must disallow fields and methods with the same name on semantic check phase. Or is this already guaranteed during name resolution?

    let field_index = Self::assert_trunc_cast(
      object_type
        .fields
        .iter()
        // OPTIMIZE: If possible, avoid `O(n)` linear search for the position.
        .position(|field| field.0 == &object_access.field_name)
        .expect(BUG_FIELD_MISSING),
    );

    let llvm_field = if llvm_struct.is_pointer_value() {
      let llvm_struct_ptr = llvm_struct.into_pointer_value();

      let target_field_type = object_type
        .fields
        .get(&object_access.field_name)
        .expect(BUG_FIELD_MISSING);

      let llvm_field_type = self.lower_type(target_field_type)?;

      let llvm_struct_type = self
        .lower_type(&types::Type::Object(object_type.to_owned()))
        .expect(BUG_TYPE_NEVER_UNIT);

      let llvm_field_gep = self
        .llvm_builder
        .build_struct_gep(
          llvm_struct_type,
          llvm_struct_ptr,
          field_index,
          "object.field.gep",
        )
        // SAFETY: Undocumented unwrap. Prefer `expect`.
        .unwrap();

      // NOTE: Since object members are unknown, we must be careful when we apply
      // access rules (need to consider cases where exceptions need to be made,
      // ex. string literals). However, in this specific case, the layer that will
      // be peeled off is not that of those exceptions, instead it is only that of the
      // LLVM GEP instruction.
      self.access_if_mode_applies(llvm_field_type, llvm_field_gep, "object.field.access")

      // TODO: We might need two versions, one for pointer objects and another for constant objects? Actually no, since we don't have constant objects?
    } else {
      self
        .llvm_builder
        .build_extract_value(
          llvm_struct.into_struct_value(),
          field_index,
          "object.field.extract",
        )
        // REVISE: Prefer `expect`, and consider using a const global reason.
        .unwrap()
    };

    Some(llvm_field)
  }

  fn visit_object(
    &mut self,
    object: &ast::Object,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_struct_type = self
      .lower_type_by_id(&object.type_id)
      .expect(lowering_ctx::BUG_TYPE_NEVER_UNIT)
      .into_struct_type();

    let llvm_struct_alloca = self.alloca(llvm_struct_type, "object.alloca");

    // OPTIMIZE: Avoid cloning (only cloning to satisfy borrow checker).
    let object_type_type = self.resolve_type_by_id(&object.type_id).into_owned();

    let object_type_fields = match object_type_type {
      types::Type::Object(types::ObjectType { fields, .. }) => fields,
      _ => unreachable!("object item's associated type should always be an object type"),
    };

    let field_types = object_type_fields
      .iter()
      // SAFETY: Undocumented unwrap.
      .map(|type_field| object.fields.get(type_field.0).unwrap());

    for (index, field_value) in field_types.enumerate() {
      let llvm_field_value = self
        .lower_with_access_mode(&field_value, lowering_ctx::AccessMode::Value)
        .unwrap_or_else(|| self.make_llvm_unit_value().as_basic_value_enum());

      // BUG: (tag:unit-value) What about when unit value is used as part of fields? In fact, the unit value can be used anywhere and cause logic issues! There needs to be a consistent framework to handle them, and not rely on superficial index positions to avoid logic bugs.

      let struct_field_gep = self
        .llvm_builder
        .build_struct_gep(
          llvm_struct_type,
          llvm_struct_alloca,
          Self::assert_trunc_cast(index),
          "object.alloca.field.gep",
        )
        // SAFETY: Undocumented unwrap. Prefer `expect`.
        .unwrap();

      self
        .llvm_builder
        // FIXME: For nested structs, they will return `None`.
        .build_store(struct_field_gep, llvm_field_value)
        .expect(BUG_BUILDER_UNSET);
    }

    // Access struct value if used as a let-binding's value, otherwise we'd be
    // trying to store *StructValue -> *StructValue, which is not allowed in LLVM
    // IR; it should be StructValue -> *StructValue.
    Some(self.access_if_mode_applies(
      llvm_struct_type.as_basic_type_enum(),
      llvm_struct_alloca,
      "object",
    ))
  }

  fn visit_foreign_cluster(
    &mut self,
    foreign_cluster: &ast::ForeignCluster,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    for foreign in &foreign_cluster.foreigns {
      self.visit_item(foreign);
    }

    None
  }

  fn visit_foreign_var(
    &mut self,
    foreign_var: &ast::ForeignStatic,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // If the type is unit, nothing should be emitted.
    let llvm_type = self.lower_type(&foreign_var.ty)?;

    let memoization_key = (self.access_mode, foreign_var.registry_id);

    if let Some(llvm_cached_value) = self.llvm_value_memoization.get(&memoization_key) {
      // BUG: (tag:access) Access rules aren't taken into account for memoized values. This means that if the value is memoized without access, but then referenced again (binding) with intent to access as a value, a possible pointer (alloca) will be returned and nothing is accessed. However, access rules shouldn't simply apply if possible here either; certain values MUST choose whether to access or not! This bug also applies to all other instances of memoization, some of which are being 'naively' accessed.
      return *llvm_cached_value;
    }

    // NOTE: Foreign statics can be lowered more than once; depending on what
    // access mode is present during lowering. For example, a foreign static
    // pointer could be used as a pointer, or dereferenced to be used as a value.
    let llvm_global_value = self
      .llvm_module
      .add_global(llvm_type, None, &foreign_var.name)
      .as_basic_value_enum();

    self
      .llvm_value_memoization
      .insert(memoization_key, Some(llvm_global_value));

    Some(llvm_global_value)
  }

  fn visit_parameter(
    &mut self,
    parameter: &ast::Parameter,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // If the parameter's type is unit, nothing should be emitted.
    if self.resolve_type_by_id(&parameter.type_id).is_a_unit() {
      return None;
    }

    let llvm_parameter = self
      .llvm_function_buffer
      .expect(auxiliary::BUG_BUFFER_CONTRACT)
      // NOTE: In the case that the function that owns this parameter
      // is a foreign function with variadic parameters, then this
      // parameter cannot be part of the variadic parameters, since
      // it would need to be present in the AST to be processed as
      // a parameter, and variadic parameters are all represented simply
      // as a single flag in the signature, not actual parameter AST
      // nodes.
      .get_nth_param(Self::assert_trunc_cast(parameter.position))
      // REVISE: Use `expect`.
      .unwrap();

    Some(llvm_parameter)
  }

  fn visit_binary_op(
    &mut self,
    binary_op: &ast::BinaryOp,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_left_operand = self
      .lower_with_access_mode(&binary_op.left_operand, lowering_ctx::AccessMode::Value)
      .expect(lowering_ctx::BUG_LLVM_VALUE);

    let llvm_right_operand = self
      .lower_with_access_mode(&binary_op.right_operand, lowering_ctx::AccessMode::Value)
      .expect(lowering_ctx::BUG_LLVM_VALUE);

    let operand_type = self.resolve_type_by_id(&binary_op.operand_type_id);

    // NOTE: After type unification and semantic analysis phase, it is
    // assumed that both operands are of the same type; otherwise a diagnostic
    // would have previously been emitted.
    let both_operands_are_of_type_int = matches!(
      operand_type.as_ref(),
      types::Type::Primitive(types::PrimitiveType::Integer(..))
    );

    // A runtime assertion guard must be inserted on all division operations.
    if binary_op.operator == ast::BinaryOperator::Divide {
      let llvm_zero = self
        .llvm_module
        .get_context()
        .i32_type()
        .const_zero()
        .as_basic_value_enum();

      let llvm_is_zero = if both_operands_are_of_type_int {
        self
          .llvm_builder
          .build_int_compare(
            inkwell::IntPredicate::EQ,
            llvm_right_operand.into_int_value(),
            llvm_zero.into_int_value(),
            "int.div_op.is_zero",
          )
          .expect(BUG_BUILDER_UNSET)
          .as_basic_value_enum()
      } else {
        self
          .llvm_builder
          .build_float_compare(
            inkwell::FloatPredicate::OEQ,
            llvm_right_operand.into_float_value(),
            llvm_zero.into_float_value(),
            "float.div_op.is_zero",
          )
          .expect(BUG_BUILDER_UNSET)
          .as_basic_value_enum()
      };

      let llvm_is_not_zero = self
        .llvm_builder
        .build_not(llvm_is_zero.into_int_value(), "div_op.is_not_zero")
        .expect(BUG_BUILDER_UNSET);

      self.insert_runtime_guard(llvm_is_not_zero, lowering_ctx::RuntimeGuard::DivisionByZero);
    }

    // REVISE: Simplify this to obtain the operator/predicate, then lower separately? Maybe not possible.
    let llvm_operation = match binary_op.operator {
      ast::BinaryOperator::Add if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_add(
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.add_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Add => self
        .llvm_builder
        .build_float_add(
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.add_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Subtract if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_sub(
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.subtract_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Subtract => self
        .llvm_builder
        .build_float_sub(
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.subtract_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Multiply if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_mul(
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.multiply_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Multiply => self
        .llvm_builder
        .build_float_mul(
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.multiply_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      // TODO: Support for unsgined division?
      ast::BinaryOperator::Divide if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_signed_div(
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.divide_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Divide => self
        .llvm_builder
        .build_float_div(
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.divide_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::LessThan if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_compare(
          // TODO: Support for unsigned?
          inkwell::IntPredicate::SLT,
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.slt_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::LessThan => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OLT,
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.slt_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::GreaterThan if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_compare(
          // TODO: Support for unsigned?
          inkwell::IntPredicate::SGT,
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.sgt_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::GreaterThan => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OGT,
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.gt_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::LessThanOrEqual if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::SLE,
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.sltoe_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::LessThanOrEqual => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OLE,
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.ltoe_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::GreaterThanOrEqual if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::SGE,
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.sgtoe_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::GreaterThanOrEqual => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OGE,
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.gtoe_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      // TODO: Equality for composite types (structs).
      ast::BinaryOperator::Equality if both_operands_are_of_type_int => self
        .llvm_builder
        .build_int_compare(
          inkwell::IntPredicate::EQ,
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "int.eq_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Equality => self
        .llvm_builder
        .build_float_compare(
          inkwell::FloatPredicate::OEQ,
          llvm_left_operand.into_float_value(),
          llvm_right_operand.into_float_value(),
          "float.eq_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::And => self
        .llvm_builder
        .build_and(
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "and_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Or => self
        .llvm_builder
        .build_or(
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "or_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Nand => self
        .llvm_builder
        .build_not(
          self
            .llvm_builder
            .build_and(
              llvm_left_operand.into_int_value(),
              llvm_right_operand.into_int_value(),
              "",
            )
            .expect(BUG_BUILDER_UNSET),
          "nand_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Nor => self
        .llvm_builder
        .build_not(
          self
            .llvm_builder
            .build_or(
              llvm_left_operand.into_int_value(),
              llvm_right_operand.into_int_value(),
              "or_op",
            )
            .expect(BUG_BUILDER_UNSET),
          "nor_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      ast::BinaryOperator::Xor => self
        .llvm_builder
        .build_xor(
          llvm_left_operand.into_int_value(),
          llvm_right_operand.into_int_value(),
          "xor_op",
        )
        .expect(BUG_BUILDER_UNSET)
        .as_basic_value_enum(),
      // TODO: Support for when comparing equality of pointers/references.
      // TODO: Support for all operators.
      _ => todo!(),
    };

    Some(llvm_operation)
  }

  fn visit_reference(
    &mut self,
    reference: &ast::Reference,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let target = self
      .symbol_table
      .follow_link(&reference.path.link_id)
      .expect(auxiliary::BUG_NAME_RESOLUTION);

    self.visit_item(&target.into_item().expect("target should be an item"))
  }

  fn visit_if(&mut self, if_: &ast::If) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_condition = self
      // NOTE: Since the condition will be treated as a value, it should be
      // accessed if applicable.
      .lower_with_access_mode(&if_.condition, lowering_ctx::AccessMode::Value)
      .expect(lowering_ctx::BUG_LLVM_VALUE)
      .into_int_value();

    let llvm_initial_block = self.get_current_block();

    let llvm_current_function = self
      .llvm_function_buffer
      .expect(auxiliary::BUG_BUFFER_CONTRACT);

    let mut llvm_yield_value_alloca = None;
    let ty = self.resolve_type_by_id(&if_.type_id);
    let llvm_type = self.lower_type(&ty);

    // Allocate the resulting if-value early on, if applicable.
    // The allocation will not occur if the type is unit.
    if let Some(llvm_if_value_type) = llvm_type {
      // FIXME: Why is the value being allocated in some cases where the type of the if is `Unit`? See test `binding`.
      llvm_yield_value_alloca = Some(self.alloca(llvm_if_value_type, "if.value"));
    }

    let llvm_context = self.llvm_module.get_context();
    let llvm_after_block = llvm_context.append_basic_block(llvm_current_function, "if.after");

    // REVISE: Move the creation of each individual block inside the elif branches loop. This way we save an iteration, and we no longer have to deal with indexing.
    // These blocks contain the branching logic for jumping to the next else
    // if guard block.
    let llvm_elif_guard_blocks = if_
      .elif_branches
      .iter()
      .map(|_| llvm_context.append_basic_block(llvm_current_function, "if.elif.check"))
      .collect::<Vec<_>>();

    // Select the detour block. This will be the block onto which the jump will
    // go to if the initial condition fails for the then branch.
    let llvm_detour_block = self.build_if_detour_block(
      if_,
      llvm_current_function,
      &llvm_yield_value_alloca,
      llvm_after_block,
      llvm_initial_block,
    );

    // First, lower the `then` branch.
    let llvm_then_block = self.build_then_branch(
      &if_,
      llvm_current_function,
      llvm_yield_value_alloca,
      llvm_after_block,
    );

    // Build the initial, entry jump into the `if` construct.
    self.llvm_builder.position_at_end(llvm_initial_block);

    self
      .llvm_builder
      .build_conditional_branch(
        llvm_condition,
        llvm_then_block,
        // Connect the alternative block if the condition fails to jump to the
        // `then` branch, to either the first else-if guard block (if any else-if
        // branches exist) or to the detour block otherwise.
        if let Some(first_elif_guard_block) = llvm_elif_guard_blocks.first() {
          *first_elif_guard_block
        } else {
          llvm_detour_block
        },
      )
      .expect(BUG_BUILDER_UNSET);

    // Next, process the else-if branches.
    for (index, branch) in if_.elif_branches.iter().enumerate() {
      self.build_elif_branch(
        branch,
        index,
        llvm_current_function,
        &llvm_elif_guard_blocks,
        &llvm_detour_block,
        llvm_after_block,
        &llvm_yield_value_alloca,
      );
    }

    // Leave the after block as current for further processing.
    self.llvm_builder.position_at_end(llvm_after_block);

    // If an expression is to be yielded, it must be accessed. A pointer
    // shouldn't be yielded. This will peel the pointer layer of the if-value alloca.
    llvm_yield_value_alloca.and_then(|llvm_yield_value_alloca| {
      llvm_type.map(|ty| self.force_access(ty, llvm_yield_value_alloca, "if.value"))
    })
  }

  fn visit_foreign_function(
    &mut self,
    foreign_fn: &ast::ForeignFunction,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let memoization_key = (lowering_ctx::AccessMode::None, foreign_fn.registry_id);

    if let Some(llvm_memoized_value) = self.llvm_value_memoization.get(&memoization_key) {
      // NOTE: LLVM value cloning is cheap.
      return *llvm_memoized_value;
    }

    // REVISE: Use another method to check whether the value was lowered (checking the LLVM module's functions instead)?
    assert!(
      !self.llvm_value_memoization.contains_key(&memoization_key),
      "the same foreign function should not be lowered twice"
    );

    let signature_type = foreign_fn
      .signature
      .as_signature_type(
        foreign_fn
          .signature
          .return_type_hint
          .as_ref()
          .expect(auxiliary::BUG_FOREIGN_FN_TYPE_HINTS)
          .to_owned(),
        self.resolution_helper,
        self.universe_stack.clone(),
      )
      .expect(auxiliary::BUG_MISSING_TYPE);

    let llvm_signature_type = self.lower_signature_type(&signature_type, None);

    assert!(
      self.llvm_module.get_function(&foreign_fn.name).is_none(),
      "foreign functions should only be lowered and defined once"
    );

    let llvm_extern = self.llvm_module.add_function(
      // NOTE: Extern names should never be mangled.
      &foreign_fn.name,
      llvm_signature_type,
      Some(inkwell::module::Linkage::External),
    );

    let result = Some(llvm_extern.as_global_value().as_basic_value_enum());

    self.llvm_value_memoization.insert(memoization_key, result);

    result
  }

  fn visit_cast(&mut self, cast: &ast::Cast) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_operand = self
      .lower_with_access_mode(&cast.operand, lowering_ctx::AccessMode::Value)
      .expect(lowering_ctx::BUG_LLVM_VALUE);

    let llvm_cast_type = self
      .lower_type(&cast.cast_type)
      .expect(lowering_ctx::BUG_TYPE_NEVER_UNIT);

    let operand_type = self.resolve_type_by_id(&cast.operand_type_id);
    let cast_type = self.resolve_type_by_id(&cast.type_id);

    let is_downcast = Self::find_bit_width(&operand_type)
      .and_then(|operand_type_bw| {
        Self::find_bit_width(&cast_type).map(|cast_type_bw| operand_type_bw > cast_type_bw)
      })
      .unwrap_or(false);

    // REVISE: Check if it's a primitive type first.
    let llvm_op_code = if is_downcast {
      // TODO: Need to handle also `FPTrunc`.
      inkwell::values::InstructionOpcode::Trunc
    } else if let types::Type::Primitive(primitive_cast_type) = cast_type.as_ref() {
      // The operand type must be a primitive type as well.
      let primitive_operand_type = assert_extract!(operand_type.as_ref(), types::Type::Primitive);

      // At this point, the cast is an upcast.
      match (primitive_operand_type, primitive_cast_type) {
        (
          types::PrimitiveType::Integer(.., is_signed_a),
          types::PrimitiveType::Integer(.., is_signed_b),
        ) if is_signed_a != is_signed_b => inkwell::values::InstructionOpcode::SExt,
        (types::PrimitiveType::Integer(..), types::PrimitiveType::Integer(..)) => {
          inkwell::values::InstructionOpcode::ZExt
        }
        (types::PrimitiveType::Real(..), types::PrimitiveType::Real(..)) => {
          inkwell::values::InstructionOpcode::FPExt
        }
        (types::PrimitiveType::Real(..), types::PrimitiveType::Integer(.., true)) => {
          inkwell::values::InstructionOpcode::FPToSI
        }
        (types::PrimitiveType::Real(..), types::PrimitiveType::Integer(.., false)) => {
          inkwell::values::InstructionOpcode::FPToUI
        }
        (types::PrimitiveType::Integer(.., true), types::PrimitiveType::Real(..)) => {
          inkwell::values::InstructionOpcode::SIToFP
        }
        (types::PrimitiveType::Integer(.., false), types::PrimitiveType::Real(..)) => {
          inkwell::values::InstructionOpcode::UIToFP
        }
        // TODO: Continue implementation.
        _ => unreachable!("incompatible casts should have been previously detected and handled"),
      }
    }
    // Otherwise, the cast is between pointers.
    else {
      inkwell::values::InstructionOpcode::BitCast
    };

    let llvm_cast_inst = self
      .llvm_builder
      .build_cast(llvm_op_code, llvm_operand, llvm_cast_type, "cast_op")
      .expect(BUG_BUILDER_UNSET);

    // TODO: Finish implementation.

    Some(llvm_cast_inst)
  }

  fn visit_literal(
    &mut self,
    literal: &ast::Literal,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_context = self.llvm_module.get_context();

    Some(match &literal.kind {
      ast::LiteralKind::String(value) => self.intern_or_get_string(value, "string_literal"),
      ast::LiteralKind::Char(value) => llvm_context
        .i8_type()
        // NOTE: It is safe to cast from `char` to `u64` because the
        // `char` type is guaranteed to be a 32-bit Unicode scalar value.
        .const_int(value.clone() as u64, false)
        .as_basic_value_enum(),
      ast::LiteralKind::Bool(value) => llvm_context
        .bool_type()
        .const_int(value.clone() as u64, false)
        .as_basic_value_enum(),
      ast::LiteralKind::Nullptr(..) => llvm_context
        // NOTE: The pointee type no longer matters in this specific case;
        // LLVM 15 has replaced all pointer types with opaque pointers.
        .bool_type()
        .ptr_type(inkwell::AddressSpace::default())
        .const_null()
        .as_basic_value_enum(),
      ast::LiteralKind::Number {
        value,
        is_real,
        bit_width,
        ..
      } => {
        let llvm_float_type = || match bit_width {
          types::BitWidth::Width16 => llvm_context.f16_type(),
          types::BitWidth::Width32 => llvm_context.f32_type(),
          types::BitWidth::Width64 => llvm_context.f64_type(),
          // TODO: Only should be present if the value isn't a real.
          _ => llvm_context.f16_type(),
        };

        let llvm_int_type = || {
          llvm_context.custom_width_int_type(match bit_width {
            types::BitWidth::Width8 => 8,
            types::BitWidth::Width16 => 16,
            types::BitWidth::Width32 => 32,
            types::BitWidth::Width64 => 64,
            types::BitWidth::Width128 => 128,
          })
        };

        if *is_real {
          llvm_float_type().const_float(*value).as_basic_value_enum()
        } else {
          llvm_int_type()
            // NOTE: Integers can be negated using the minus operator. They are always
            // lowered as unsigned here. The cast is also safe because the fractional
            // component of the number is not being used, thus it is safe to drop it.
            .const_int(*value as u64, false)
            .as_basic_value_enum()
        }
      }
    })
  }

  fn visit_unary_op(
    &mut self,
    unary_op: &ast::UnaryOp,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    Some(match unary_op.operator {
      ast::UnaryOperator::ReferenceOf => {
        let llvm_value = self
          .lower_with_access_mode(&unary_op.operand, lowering_ctx::AccessMode::None)
          .expect(lowering_ctx::BUG_LLVM_VALUE);

        // If the lowered operand did not yield a pointer value, then it means
        // that a stack allocation must occur for the value, because the value likely
        // was a literal, but an LLVM pointer value is needed for references.
        if !llvm_value.is_pointer_value() {
          let llvm_stack_allocation =
            self.alloca(llvm_value.get_type(), "reference.literal.alloca");

          self
            .llvm_builder
            .build_store(llvm_stack_allocation, llvm_value)
            .expect(BUG_BUILDER_UNSET);

          llvm_stack_allocation.as_basic_value_enum()
        } else {
          llvm_value
        }
      }
      ast::UnaryOperator::Not => {
        let llvm_value = self
          .lower_with_access_mode(&unary_op.operand, lowering_ctx::AccessMode::Value)
          .expect(lowering_ctx::BUG_LLVM_VALUE);

        self
          .llvm_builder
          // NOTE: The value's type is assumed to be a boolean (`i1`). This should be enforced
          // during type-checking.
          .build_not(llvm_value.into_int_value(), "not_op")
          .expect(BUG_BUILDER_UNSET)
          .as_basic_value_enum()
      }
      ast::UnaryOperator::Negate => {
        let llvm_value = self
          .lower_with_access_mode(&unary_op.operand, lowering_ctx::AccessMode::Value)
          .expect(lowering_ctx::BUG_LLVM_VALUE);

        if llvm_value.is_int_value() {
          self
            .llvm_builder
            // NOTE: The value's type is assumed to be a boolean (`i1`). This should be enforced
            // during type-checking.
            .build_int_neg(llvm_value.into_int_value(), "int.negate_op")
            .expect(BUG_BUILDER_UNSET)
            .as_basic_value_enum()
        }
        // Otherwise, it must be a float.
        else {
          self
            .llvm_builder
            .build_float_neg(llvm_value.into_float_value(), "float.negate_op")
            .expect(BUG_BUILDER_UNSET)
            .as_basic_value_enum()
        }
      }
      ast::UnaryOperator::Dereference => {
        let operand_type = self
          .resolution_helper
          .resolve_by_id(&unary_op.operand_type_id, self.universe_stack.clone())
          .expect(auxiliary::BUG_MISSING_TYPE);

        let pointee_type = assert_extract!(operand_type.as_ref(), types::Type::Pointer);
        let llvm_pointe_type = self.lower_type(&pointee_type)?;

        let llvm_pointer_value = self
          .lower_with_access_mode(&unary_op.operand, lowering_ctx::AccessMode::None)
          .expect(lowering_ctx::BUG_LLVM_VALUE)
          .into_pointer_value();

        let llvm_is_null = self
          .llvm_builder
          .build_is_null(llvm_pointer_value, "is_null")
          .expect(BUG_BUILDER_UNSET)
          .as_basic_value_enum();

        let llvm_is_not_null = self
          .llvm_builder
          .build_int_compare(
            inkwell::IntPredicate::EQ,
            llvm_is_null.into_int_value(),
            self.llvm_module.get_context().bool_type().const_zero(),
            "is_not_null",
          )
          .expect(BUG_BUILDER_UNSET);

        // All dereference operations must be checked for null pointers.
        // Dereferencing a null pointer is undefined behavior.
        self.insert_runtime_guard(
          llvm_is_not_null,
          lowering_ctx::RuntimeGuard::NullDereference,
        );

        // BUG: (tag:access) If the value is a reference to a let-statement, the pointer of the let-statement will be removed, but the actual pointer value will not be accessed. Perhaps will need to remove special treatment of let-statements.
        self.force_access(llvm_pointe_type, llvm_pointer_value, "dereference_op")
      }
    })
  }

  fn visit_binding(
    &mut self,
    binding: &ast::Binding,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let memoization_key = (self.access_mode, binding.registry_id);

    if let Some(llvm_cached_value) = self.llvm_value_memoization.get(&memoization_key) {
      return *llvm_cached_value;
    }

    // OPTIMIZE: Avoid cloning; only cloning to satisfy borrow checker.
    let value_type = self.resolve_type_by_id(&binding.type_id).into_owned();

    // BUG: (tag:hof-lowering) When lowering higher-order functions as binding values, their types are lowered as LLVM pointer to function types, since LLVM function types cannot be represented as basic types. Since this is the case, when the access occurs (during memo retrieval or otherwise), its pointee type remains as a pointer, which causes the problem of `expected FunctionValue, got PointerValue` during call site lowering of the callee. This portion is correct; higher-order functions must remain as pointer to function types, the logic that needs to be changed is in the call site lowering. When lowering higher-order functions, use `build_indirect_call` to build calls to function pointers.
    // The value type must not be unit in order to continue,
    // otherwise the value will never evaluate, so there is
    // nothing to allocate for the binding.
    let llvm_value_type = self.lower_type(&value_type)?;

    let llvm_value = self
      .visit_expr(&binding.value)
      .expect(lowering_ctx::BUG_LLVM_VALUE)
      .as_basic_value_enum();

    let is_reference = matches!(value_type, types::Type::Reference(_));

    let llvm_final_value = if is_reference {
      let llvm_alloca = self.alloca(llvm_value_type, "binding.reference.alloca");

      self
        .llvm_builder
        .build_store(llvm_alloca, llvm_value)
        .expect(BUG_BUILDER_UNSET);

      llvm_alloca.as_basic_value_enum()
    } else {
      llvm_value
    };

    self
      .llvm_value_memoization
      .insert(memoization_key, Some(llvm_final_value));

    // NOTE: Since bindings are declarations themselves, they yield values.
    Some(llvm_final_value)
  }

  fn visit_call_site(
    &mut self,
    call_site: &ast::CallSite,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // REVISE: Break function apart and avoid repeated code.

    let mut llvm_arguments = call_site
      .arguments
      .iter()
      .map(|argument| {
        self
          .lower_with_access_mode(&argument.value, lowering_ctx::AccessMode::Value)
          .unwrap_or_else(|| self.make_llvm_unit_value().as_basic_value_enum())
          .into()
      })
      .collect::<Vec<_>>();

    // Save and restore buffers after the callee has been processed to avoid
    // context contamination.
    let previous_state = self.save_state();

    let callee = call_site.strip_callee(self.symbol_table).unwrap();
    let mut closure_captures = None;

    // If the callee is a closure with captures, an environment argument must
    // be appended to the argument list.
    if let ast::Callable::Closure(closure) = &callee {
      if !closure.captures.is_empty() {
        let llvm_captures_env = self
          .build_call_site_closure_env(&closure.captures)
          .as_basic_value_enum()
          .into();

        llvm_arguments.push(llvm_captures_env);
        closure_captures = Some(&closure.captures);
      }
    }

    let callee_function = match &callee {
      ast::Callable::Function(function) => Some(function),
      _ => None,
    };

    let mut llvm_cached_monomorphic_fn = None::<inkwell::values::FunctionValue<'llvm>>;

    let own_universe_stack = resolution::push_to_universe_stack(
      self.universe_stack.clone(),
      call_site.universe_id.to_owned(),
    )
    .unwrap();

    // OPTIMIZE: Not used in all final branches, but cannot be made a closure because of unique access requirement to `self`.
    let argument_types = {
      call_site
        .arguments
        .iter()
        .map(|argument| {
          self
            .resolution_helper
            .resolve_by_id(&argument.type_id, own_universe_stack.clone())
            .map(|ty| ty.into_owned())
            .expect(auxiliary::BUG_MISSING_TYPE)
        })
        .collect::<Vec<_>>()
    };

    // If the callee is a function that is cached on the monomorphism
    // cache, attempt to retrieve a matching entry, if it exists.
    if let Some(callee_function) = callee_function {
      if let Some(llvm_cached_monomorphism) =
        self.find_memoized_monomorphism(&callee_function.registry_id, &argument_types)
      {
        assert!(
          callee_function.is_polymorphic(),
          "all functions in the monomorphism cache should be polymorphic"
        );

        llvm_cached_monomorphic_fn = Some(llvm_cached_monomorphism)
      }
    }

    // If no cached monomorphism was found, attempt to instantiate the callee
    // if it is a polymorphic function.
    let llvm_monomorphic_fn = if llvm_cached_monomorphic_fn.is_none() {
      callee_function.and_then(|callee_function| {
        if callee_function.is_polymorphic() {
          let monomorphic_function = self
            .lower_artifact(
              call_site.universe_id.to_owned(),
              &ast::Item::Function(std::rc::Rc::clone(callee_function)),
            )
            .expect(BUG_LLVM_VALUE)
            .as_any_value_enum()
            .into_function_value();

          Some(monomorphic_function)
        } else {
          None
        }
      })
    } else {
      None
    };

    let callee_type = self
      .resolve_type_by_id(&call_site.callee_type_id)
      .into_owned();

    // BUG: (tag:closure-captures) When building an indirect call to a closure that has captures (and thus a capture environment), its signature/function type is lowered without the environment parameter. This causes internal LLVM issues and produces misleading LLVM errors, even module verification! The callee type must be modified and the capture environment be taken into account.
    let callee_signature_type = assert_extract!(callee_type, types::Type::Signature);

    // CONSIDER: Breaking this down into separate helper functions.
    let llvm_call = if let Some(llvm_cached_monomorphic_fn) = llvm_cached_monomorphic_fn {
      self
        .llvm_builder
        .build_direct_call(
          llvm_cached_monomorphic_fn,
          llvm_arguments.as_slice(),
          "call.monomorphism.memo",
        )
        .expect(BUG_BUILDER_UNSET)
        .try_as_basic_value()
        .left()
    } else if let Some(llvm_monomorphic_fn) = llvm_monomorphic_fn {
      self.memoize_monomorphic_fn(
        callee.get_registry_id(),
        argument_types,
        llvm_monomorphic_fn,
      );

      self
        .llvm_builder
        .build_direct_call(llvm_monomorphic_fn, llvm_arguments.as_slice(), "call")
        .expect(BUG_BUILDER_UNSET)
        .try_as_basic_value()
        .left()
    } else {
      // NOTE: The callee will always be a pointer at this point, because
      // it is not possible to have an LLVM function as an inline-value.
      let llvm_callee_ptr = self
        .lower_with_access_mode(&call_site.callee_expr, lowering_ctx::AccessMode::None)
        .expect(lowering_ctx::BUG_LLVM_VALUE)
        .into_pointer_value();

      let llvm_pointee_type = self.lower_signature_type(&callee_signature_type, closure_captures);

      self
        .llvm_builder
        .build_indirect_call(
          llvm_pointee_type,
          llvm_callee_ptr,
          llvm_arguments.as_slice(),
          "call",
        )
        .expect(BUG_BUILDER_UNSET)
        .try_as_basic_value()
        .left()
    };

    self.restore_state(previous_state);

    llvm_call
  }

  fn visit_block(&mut self, block: &ast::Block) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    for statement in &block.statements {
      // Statements will not be used as values, therefore they shouldn't be accessed.
      // This will also prevent logic bugs, since bindings are access-sensitive, and
      // the flag may be raised by any construct that may lower blocks.
      self.lower_with_access_mode(
        &ast::Expr::Statement(std::rc::Rc::clone(statement)),
        lowering_ctx::AccessMode::None,
      );

      // REVIEW: Will this ever be the case? If so, give an example in the comment. If this should never be the case, consider having an assertion.
      // Do not continue lowering statements if the current block was terminated.
      if self.get_current_block().get_terminator().is_some() {
        break;
      }
    }

    // Access the yielded value instead of returning a pointer like a stack-allocated one,
    // since such pointers can be de-allocated when the scope they're in (not limited to
    // function bodies) is exited. To avoid issues, make a copy of the value before using it.
    self.lower_with_access_mode(&block.yield_value, lowering_ctx::AccessMode::Value)
  }

  fn visit_function(
    &mut self,
    function: &ast::Function,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // TODO: Polymorphic functions should not be lowered by themselves; they can only be lowered when they are instantiated. Currently, if a polymorphic function isn't called, it will be lowered and then when its parameter types are lowered, it will throw an error because generic types cannot be lowered.

    // REVIEW: If we do choose to lower declarations such as functions on-demand (i.e. a reference to a function yet to be lowered), buffers would interfere with the lowering of such function, so we would need to stash buffers and pop them when entering and exiting functions.

    let is_polymorphic = function.is_polymorphic();
    let memoization_key = (lowering_ctx::AccessMode::None, function.registry_id);

    if let Some(llvm_cached_value) = self.llvm_value_memoization.get(&memoization_key) {
      assert!(
        !is_polymorphic,
        "non-polymorphic memoization cache should not contain polymorphic functions"
      );

      // NOTE: Functions are never accessed, thus access rules will never apply
      // to cached LLVM functions.
      return *llvm_cached_value;
    }

    // REVISE: Use another method to check whether the value was lowered (checking the LLVM module's functions instead)?
    assert!(
      !self.llvm_value_memoization.contains_key(&memoization_key),
      "all non-polymorphic functions should be lowered at most once"
    );

    let return_type = self
      .resolve_type_by_id(&function.signature.return_type_id)
      .into_owned();

    let is_return_type_unit = return_type.is_a_unit();

    let signature_type = function
      .signature
      .as_signature_type(
        return_type,
        self.resolution_helper,
        self.universe_stack.clone(),
      )
      .expect(auxiliary::BUG_MISSING_TYPE);

    let llvm_function_type = self.lower_signature_type(&signature_type, None);
    let is_entry_point = function.name == ENTRY_POINT_NAME;

    // Name mangling should not be applied to the entry function.
    let llvm_function_name = if is_entry_point {
      // OPTIMIZE: Any way to avoid cloning?
      function.name.to_owned()
    } else {
      self.mangle_name(&function.name)
    };

    // The entry function must be externally visible. All other functions
    // default to private linkage (current module).
    let linkage = if is_entry_point {
      inkwell::module::Linkage::External
    } else {
      inkwell::module::Linkage::Private
    };

    let llvm_function =
      self
        .llvm_module
        .add_function(&llvm_function_name, llvm_function_type, Some(linkage));

    let previous_state = self.save_state();

    self.llvm_function_buffer = Some(llvm_function);

    // FIXME: The same procedure will need to be done to allow recursion, but for polymorphic functions!
    // If the function is not polymorphic, cache its LLVM function value
    // before visiting the body, to allow for recursive function calls.
    if !is_polymorphic {
      self.llvm_value_memoization.insert(
        memoization_key,
        Some(llvm_function.as_global_value().as_basic_value_enum()),
      );
    }

    // Assign names to the LLVM parameters. This is useful for debugging.
    for parameters in llvm_function
      .get_param_iter()
      .zip(function.signature.parameters.iter())
    {
      parameters
        .0
        .set_name(&format!("parameter.{}", parameters.1.name))
    }

    let llvm_entry_block = self
      .llvm_module
      .get_context()
      .append_basic_block(llvm_function, "fn.entry");

    self.llvm_entry_block = Some(llvm_entry_block);
    self.llvm_builder.position_at_end(llvm_entry_block);

    let llvm_body = self.visit_expr(&ast::Expr::Block(std::rc::Rc::clone(&function.body)));

    // REVIEW: Does this actually stop from returning bindings? What if the binding's type is not unit?
    // If the return type is unit, simply return void. Having this
    // check prevents from returning bindings if they are the body's
    // yield value (last statement), which would conflict with the void
    // return type (since lowering bindings yields LLVM values).
    if is_return_type_unit {
      self
        .llvm_builder
        .build_return(None)
        .expect(BUG_BUILDER_UNSET);
    } else {
      self.try_build_ret(llvm_body);
    }

    self.restore_state(previous_state);

    // TODO: If this is indeed converting it into a pointer value under the hood, then write a comment here noting it.
    Some(llvm_function.as_global_value().as_basic_value_enum())
  }

  fn visit_unsafe(
    &mut self,
    unsafe_: &ast::Unsafe,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    self.visit_expr(&unsafe_.0)
  }

  fn visit_closure(
    &mut self,
    closure: &ast::Closure,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // CONSIDER: This logic is very similar to the logic for lowering functions. Can we abstract it away? Perhaps use a "lower_function" helper method, with a flag parameter to specify whether it is a closure or not.

    let memoization_key = (lowering_ctx::AccessMode::None, closure.registry_id);

    if let Some(llvm_value_cache) = self.llvm_value_memoization.get(&memoization_key) {
      return *llvm_value_cache;
    }

    // REVISE: Use another method to check whether the value was lowered (checking the LLVM module's functions instead)?
    assert!(
      !self.llvm_value_memoization.contains_key(&memoization_key),
      "all closures should be lowered at most once"
    );

    let previous_state = self.save_state();

    let return_type = self
      .resolve_type_by_id(&closure.signature.return_type_id)
      .into_owned();

    let is_return_type_unit = return_type.is_a_unit();

    let signature_type = closure
      .signature
      .as_signature_type(
        return_type,
        self.resolution_helper,
        self.universe_stack.clone(),
      )
      .expect(auxiliary::BUG_MISSING_TYPE);

    let llvm_function_type = self.lower_signature_type(&signature_type, Some(&closure.captures));

    let llvm_function = self.llvm_module.add_function(
      &self.mangle_name("closure"),
      llvm_function_type,
      Some(inkwell::module::Linkage::Private),
    );

    // Manually cache the function before visiting the body, to
    // allow for recursive function calls.
    self.llvm_value_memoization.insert(
      memoization_key,
      Some(llvm_function.as_global_value().as_basic_value_enum()),
    );

    // NOTE: Closure's signatures are only modified for the insertion
    // of its capture environment, which is appended to the end, and
    // shouldn't affect the positions of other parameters. In other words,
    // all the parameters should be correctly named, since closures cannot
    // accept instances, and thus displacement should not occur.
    for (parameter, llvm_parameter) in closure
      .signature
      .parameters
      .iter()
      .zip(llvm_function.get_param_iter())
    {
      llvm_parameter.set_name(&parameter.name);
    }

    // If there are captures, name the capture environment parameter as well.
    if !closure.captures.is_empty() {
      llvm_function
        .get_last_param()
        .expect(BUG_CLOSURE_ENV_PARAM)
        .set_name("closure.environment");
    }

    self.llvm_function_buffer = Some(llvm_function);

    let llvm_entry_block = self
      .llvm_module
      .get_context()
      .append_basic_block(llvm_function, "closure.entry");

    self.llvm_builder.position_at_end(llvm_entry_block);
    self.llvm_entry_block = Some(llvm_entry_block);

    let body = self.lower_with_access_mode(&closure.body, lowering_ctx::AccessMode::Value);

    // TODO: At the end of the body, if the closure had captures, since they are heap allocated, it would be a good idea to `free()` them, at least until lifetime mechanics are implemented. But considerations must be made; what if fields from the environment where passed/moved along to other functions? Wouldn't those fields be possibly invalidated?

    // If the return type is unit, simply return void. Having this
    // check prevents from returning let-bindings if they are the body's
    // yield value (last statement), which would conflict with the void
    // return type (since lowering let-bindings yields LLVM values).
    if is_return_type_unit {
      self
        .llvm_builder
        .build_return(None)
        .expect(BUG_BUILDER_UNSET);
    } else {
      self.try_build_ret(body);
    }

    self.restore_state(previous_state);

    Some(llvm_function.as_global_value().as_basic_value_enum())
  }

  fn visit_closure_capture(
    &mut self,
    closure_capture: &ast::ClosureCapture,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_capture_type = self
      .lower_type_by_id(&closure_capture.type_id)
      .unwrap_or_else(|| self.make_llvm_unit_type().as_basic_type_enum());

    let current_closure_expr = self
      .symbol_table
      .registry
      .get(&closure_capture.closure_registry_id)
      .expect(auxiliary::BUG_NAME_RESOLUTION)
      .into_expr()
      .expect(auxiliary::BUG_REGISTRY_ITEM_MUST_BE_ITEM);

    let current_closure = assert_extract!(current_closure_expr, ast::Expr::Closure);

    assert!(
      !current_closure.captures.is_empty(),
      "current closure should have captures"
    );

    let llvm_current_closure = self
      .llvm_function_buffer
      .expect("an LLVM function buffer should be set for the current closure");

    let llvm_last_parameter = llvm_current_closure
      .get_last_param()
      .expect(BUG_CLOSURE_ENV_PARAM);

    assert!(llvm_last_parameter.is_pointer_value(), "last parameter of LLVM function buffer for current closure should be a pointer to a struct value representing the closure captures environment");

    let llvm_captures_env = llvm_last_parameter.into_pointer_value();

    // NOTE: This is only being created as the `build_struct_gep`
    // instruction requires a type; the captures environment is not
    // actually being allocated here.
    let llvm_captures_env_type = self
      .lower_type(&self.create_captures_env_type(&current_closure.captures))
      .expect(BUG_TYPE_NEVER_UNIT);

    let llvm_capture = self
      .llvm_builder
      .build_struct_gep(
        llvm_captures_env_type,
        llvm_captures_env,
        closure_capture.index,
        &format!("closure.capture.{}", closure_capture.name),
      )
      // REVISE: Use `expect`, and provide a reason why.
      .unwrap();

    // TODO: Compare against lowering of parameter to ensure nothing's missing. At least by assertions.

    // FIXME: Access rules aren't taken into account here. What if the capture was a string? Then the string would be accessed!
    Some(self.access_if_mode_applies(llvm_capture_type, llvm_capture, "closure.capture"))
  }

  fn visit_pointer_indexing(
    &mut self,
    pointer_indexing: &ast::PointerIndexing,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let pointer_type = self.resolve_type_by_id(&pointer_indexing.type_id);
    let pointee_type = assert_extract!(pointer_type.as_ref(), types::Type::Pointer);

    let llvm_pointee_type = self
      .lower_type(&pointee_type)
      // SAFETY: Although this is true, what if the type was actually specified by the user?
      // The pointee type can never be unit; unit has no size, and therefore cannot
      // be allocated.
      .expect(lowering_ctx::BUG_TYPE_NEVER_UNIT);

    let llvm_pointer = self
      // SAFETY: Ensure this is the proper way to lower pointer indexing. Is it truly redundant?
      // No need to access the pointer; it can be indexed directly. If it is accessed,
      // a redundant copy would be made.
      .lower_with_access_mode(&pointer_indexing.pointer, lowering_ctx::AccessMode::None)
      .expect(BUG_LLVM_VALUE)
      .into_pointer_value();

    let llvm_index = self
      .lower_with_access_mode(&pointer_indexing.index, lowering_ctx::AccessMode::Value)
      .expect(BUG_LLVM_VALUE)
      .into_int_value();

    let llvm_pointer_gep = unsafe {
      self
        .llvm_builder
        .build_gep(
          llvm_pointee_type,
          llvm_pointer,
          &[llvm_index],
          "pointer_indexing.gep",
        )
        .expect(BUG_BUILDER_UNSET)
    };

    Some(llvm_pointer_gep.as_basic_value_enum())
  }

  fn visit_pointer_assignment(
    &mut self,
    pointer_assignment: &ast::PointerAssignment,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    let llvm_pointer = self
      // Do not access the pointer, since it is being assigned to. If it were
      // to be accessed, it would be loaded, and that copy would be assigned to
      // instead.
      .lower_with_access_mode(&pointer_assignment.pointer, lowering_ctx::AccessMode::None)
      .expect(lowering_ctx::BUG_LLVM_VALUE)
      .into_pointer_value();

    let llvm_value = self
      // TODO: Document why the pointer is being lowered with access flag raised.
      .lower_with_access_mode(&pointer_assignment.value, lowering_ctx::AccessMode::Value)
      .expect(lowering_ctx::BUG_LLVM_VALUE);

    self
      .llvm_builder
      .build_store(llvm_pointer, llvm_value)
      .expect(BUG_BUILDER_UNSET);

    None
  }

  fn visit_constant(
    &mut self,
    constant: &ast::Constant,
  ) -> Option<inkwell::values::BasicValueEnum<'llvm>> {
    // If the constant was declared locally, lower and return
    // its value, and do not proceed to declare it globally as
    // a global value because it does not make sense; the constant
    // is only used within the local scope where it was declared.
    // It would also cause problems if it were to reference local
    // constants, especially involving binary operations.
    if constant.was_declared_locally {
      return self.lower_with_access_mode(&constant.value, lowering_ctx::AccessMode::Value);
    }

    let memoization_key = (self.access_mode, constant.registry_id);

    let llvm_type = match self.lower_type(&constant.ty) {
      Some(llvm_type) => llvm_type,
      None => {
        self.llvm_value_memoization.insert(memoization_key, None);

        return None;
      }
    };

    if let Some(llvm_cached_value) = self.llvm_value_memoization.get(&memoization_key) {
      return llvm_cached_value.map(|llvm_cached_value| {
        // BUG: (tag:access) This is a naive access; specific values are the ones who choose whether to access or not. Applying a general access regardless of the value is prone to become a logic bug!
        // The cached LLVM value should be accessed if applicable,
        // because the cached LLVM value for constants is a global
        // value pointer, and if they're used as values, it must be
        // accessed otherwise the pointer would be wrongly returned
        // instead of its value.
        self.access_if_mode_applies(
          llvm_type,
          llvm_cached_value.into_pointer_value(),
          "constant.cached",
        )
      });
    }

    let llvm_value = self
      .lower_with_access_mode(&constant.value, lowering_ctx::AccessMode::Value)
      .expect(lowering_ctx::BUG_LLVM_VALUE);

    let llvm_global_value = self.llvm_module.add_global(
      llvm_type,
      Some(inkwell::AddressSpace::from(Self::LLVM_CONST_ADDRESS_SPACE)),
      &self.mangle_name(&constant.name),
    );

    llvm_global_value.set_initializer(&llvm_value);

    self.llvm_value_memoization.insert(
      memoization_key,
      Some(llvm_global_value.as_basic_value_enum()),
    );

    // If applicable, the global pointer should be accessed,
    // if the constant is being used as a value. Otherwise, the
    // pointer itself would be returned.
    Some(self.access_if_mode_applies(llvm_type, llvm_global_value.as_pointer_value(), "constant"))
  }
}
