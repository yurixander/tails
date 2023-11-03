//! The parser is responsible for converting a stream of tokens into an abstract syntax tree (AST).
//!
//! This occurs after lexing has taken place. The resulting AST is immutable, and does not change throughout
//! the compilation process. The AST is then passed to the next stage of compilation, which is name resolution.

use crate::{
  ast::{self, Block},
  auxiliary, diagnostic, lexer, symbol_table, types,
};

pub(crate) type LlvmSize = u32;

pub enum SignatureParserInvoker {
  Function,
  ForeignFunction,
  Effect,
}

pub struct Parser {
  tokens: Vec<lexer::Token>,
  index: usize,
  id_generator: auxiliary::IdGenerator,
}

// TODO: When parsing, utility methods and general parsing techniques should take into account whitespace tokens, and comments, and simply ignore them. But the important thing is that they are currently not considered by default. Instead, they should be explicitly ignored during parsing, to avoid having to filter out whitespace, comments, etc. after lexing. Or, perhaps require that the parser is only fed a filtered version of the lexer's output (separation of concerns)?
impl Parser {
  fn precedence_of(token: &lexer::TokenKind) -> usize {
    match token {
      lexer::TokenKind::Or
      | lexer::TokenKind::Nor
      | lexer::TokenKind::Nand
      | lexer::TokenKind::Xor => 1,
      // The `And` operator should have greater precedence than the other logical
      // operators, so that when it is used alongside the others, it is predictable
      // and consistent. Many programming languages also do this.
      lexer::TokenKind::And => 2,
      // Comparison operators have greater precedence than logical operators.
      lexer::TokenKind::Equality
      | lexer::TokenKind::Inequality
      | lexer::TokenKind::LessThan
      | lexer::TokenKind::LessThanEqualTo
      | lexer::TokenKind::GreaterThan
      | lexer::TokenKind::GreaterThanEqualTo => 3,
      // Arithmetic operators have greater precedence than comparison operators.
      lexer::TokenKind::Plus | lexer::TokenKind::Minus => 4,
      // Multiplication and division have the greatest precedence.
      lexer::TokenKind::Asterisk | lexer::TokenKind::Slash => 5,
      // Everything else has a default precedence of 0. The `not` operator has no
      // defined precedence (thus defaulting to 0) because it is a unary operator.
      _ => 0,
      // REVIEW: What about other tokens such as `as`? These should have the greatest precedence?
    }
  }

  /// An utility function to determine if the given type can be constructed
  /// from the given value.
  ///
  /// This is used to determine whether a value can fit in a specific type.
  fn is_in_range<U, T: std::convert::TryFrom<U>>(value: U) -> bool {
    T::try_from(value).is_ok()
  }

  // TODO: Add more test cases for larger numbers than `0`. Also add tests cases for exactly 0, and negative integers.
  /// Determine the minimum bit-width in which a number can fit.
  fn minimum_bit_width_of(number: &f64) -> Result<types::BitWidth, String> {
    let log2_of_number = f64::log2(*number + 1_f64);

    // NOTE: This conversion is safe because the bit size should be
    // inspected as an integer rather than a floating-point number.
    let minimum_bit_size = f64::floor(log2_of_number) as u64;

    Ok(if minimum_bit_size <= types::BitWidth::Width8 as u64 {
      types::BitWidth::Width8
    } else if minimum_bit_size <= types::BitWidth::Width16 as u64 {
      types::BitWidth::Width16
    } else if minimum_bit_size <= types::BitWidth::Width32 as u64 {
      types::BitWidth::Width32
    } else if minimum_bit_size <= types::BitWidth::Width64 as u64 {
      types::BitWidth::Width64
    } else {
      return Err(String::from("number is too big to fit in the biggest size"));
    })
  }

  pub fn new(tokens: Vec<lexer::Token>) -> Self {
    // NOTE: Accepting an empty token list is a valid situation,
    // because there could be an input source file with no content,
    // which is acceptable.
    Self {
      tokens,
      index: 0,
      id_generator: auxiliary::IdGenerator::default(),
    }
  }

  pub fn parse_module(
    &mut self,
    qualifier: symbol_table::Qualifier,
  ) -> diagnostic::Maybe<ast::Module> {
    let mut items = Vec::new();

    while !self.is_at_last_token_or_past() {
      items.push(self.parse_item()?);
    }

    Ok(ast::Module {
      qualifier,
      global_items: items,
    })
  }

  /// Determine whether the given token is considered a valid binary
  /// operator.
  fn is_binary_operator_token(token_kind: &lexer::TokenKind) -> bool {
    matches!(
      token_kind,
      lexer::TokenKind::Plus
        | lexer::TokenKind::Minus
        | lexer::TokenKind::Asterisk
        | lexer::TokenKind::Slash
        | lexer::TokenKind::LessThan
        | lexer::TokenKind::GreaterThan
        | lexer::TokenKind::GreaterThanEqualTo
        | lexer::TokenKind::LessThanEqualTo
        | lexer::TokenKind::And
        | lexer::TokenKind::Or
        | lexer::TokenKind::Nand
        | lexer::TokenKind::Nor
        | lexer::TokenKind::Xor
        | lexer::TokenKind::Equality
        | lexer::TokenKind::Inequality
        | lexer::TokenKind::PercentSign
    )
  }

  pub fn get_id_count(&self) -> usize {
    self.id_generator.get_counter()
  }

  /// Determine whether the current token is a valid unary operator.
  ///
  /// If there is no current token (`EOF` has been reached), `false`
  /// is returned.
  fn is_unary_operator_token(&self) -> bool {
    let token = match self.get_token() {
      Ok(token) => token,
      Err(_) => return false,
    };

    matches!(
      token,
      lexer::TokenKind::Minus
        | lexer::TokenKind::Not
        | lexer::TokenKind::Ampersand
        | lexer::TokenKind::Asterisk
        | lexer::TokenKind::Backtick
    )
  }

  fn check_llvm_size(size: usize) -> diagnostic::Maybe {
    if Self::is_in_range::<usize, LlvmSize>(size) {
      Ok(())
    } else {
      Err(vec![diagnostic::Diagnostic::CountOrSizeTooLarge])
    }
  }

  fn get_llvm_size(size: usize) -> diagnostic::Maybe<LlvmSize> {
    LlvmSize::try_from(size).map_err(|_| vec![diagnostic::Diagnostic::CountOrSizeTooLarge])
  }

  fn skip_comma(&mut self, terminator: &lexer::TokenKind) -> diagnostic::Maybe {
    if !self.is(&terminator) && !self.peek_is(&terminator) {
      self.skip_one(&lexer::TokenKind::Comma)?;
    }

    Ok(())
  }

  /// Attempt to eat/consume a token of the given kind.
  ///
  /// If the token is not present, a diagnostic will be returned.
  fn skip_one(&mut self, token_kind: &lexer::TokenKind) -> diagnostic::Maybe {
    if self.is(token_kind) {
      self.skip()
    } else {
      Err(self.expected(&format!("token `{:?}`", token_kind)))
    }
  }

  fn skip_many(&mut self, token_kinds: &[lexer::TokenKind]) -> diagnostic::Maybe {
    for token_kind in token_kinds {
      self.skip_one(token_kind)?;
    }

    Ok(())
  }

  /// Create an `unexpected token` diagnostic with the given message.
  fn expected(&self, expected: &str) -> Vec<diagnostic::Diagnostic> {
    let actual_string = match self.get_token() {
      Ok(token) => format!("{:?}", token),
      Err(_) => String::from("end of file"),
    };

    vec![diagnostic::Diagnostic::ExpectedButGotToken(
      expected.to_string(),
      // CONSIDER: Implementing proper `to_string` for `TokenKind`. Only do this if it is needed/used in multiple places.
      actual_string,
    )]
  }

  /// Returns whether a loop should *continue* iterating until the given
  /// token kind is reached.
  ///
  /// If `EOF` is reached, a corresponding error variant diagnostic is returned,
  /// otherwise the [`Ok`] variant is returned, indicating
  /// whether the parser has NOT reached the given token kind.
  fn until(&self, token_kind: &lexer::TokenKind) -> diagnostic::Maybe<bool> {
    if self.is_at_last_token_or_past() {
      Err(vec![diagnostic::Diagnostic::UnexpectedlyReachedEndOfFile])
    } else {
      Ok(!self.is(token_kind))
    }
  }

  fn until_terminator(&mut self, terminator: &lexer::TokenKind) -> diagnostic::Maybe<bool> {
    self
      .until(terminator)
      .and_then(|did_not_reached_terminator| {
        let reached_terminator = !did_not_reached_terminator;

        if reached_terminator {
          self.skip_one(terminator)?;
        }

        Ok(did_not_reached_terminator)
      })
  }

  /// Retrieve the token at the current index position.
  ///
  /// If there is no token at the current index (ie. when the index
  /// is out of bounds, or `EOF` has been reached), a corresponding
  /// diagnostic is returned instead.
  fn get_token(&self) -> diagnostic::Maybe<&lexer::TokenKind> {
    if let Some(token) = self.tokens.get(self.index) {
      Ok(&token.0)
    } else {
      Err(vec![diagnostic::Diagnostic::UnexpectedlyReachedEndOfFile])
    }
  }

  /// Compare the current token kind to the given one for equality.
  ///
  /// If the index is out of bounds or there is no current token,
  /// `EOF` will be compared with the given token kind.
  fn is(&self, token_kind: &lexer::TokenKind) -> bool {
    let current_token_kind = match self.get_token() {
      Ok(token) => token,
      Err(_) => return false,
    };

    current_token_kind == token_kind
  }

  /// Attempt to reposition the index to the next token (if any).
  ///
  /// Once the index reaches the length of the token list + 1, any
  /// attempt to skip will result in an `Err` being returned, indicating
  fn skip(&mut self) -> diagnostic::Maybe {
    if !self.is_index_out_of_bounds() {
      self.index += 1;

      Ok(())
    } else {
      Err(vec![diagnostic::Diagnostic::UnexpectedlyReachedEndOfFile])
    }
  }

  /// Determine whether the index representing `EOF` has been reached.
  /// This index corresponds to `len(tokens)`, and is out of bounds.
  fn is_index_out_of_bounds(&self) -> bool {
    self.index >= self.tokens.len()
  }

  /// Retrieve the upcoming token (if any).
  fn peek(&self) -> Option<&lexer::TokenKind> {
    self.tokens.get(self.index + 1).map(|token| &token.0)
  }

  /// Compare the upcoming token to the given token.
  ///
  /// If there are no more tokens, the given token will be compared with
  /// `EOF`.
  fn peek_is(&self, token_kind: &lexer::TokenKind) -> bool {
    let next_token_kind = match self.peek() {
      Some(token_kind) => token_kind,
      None => return false,
    };

    next_token_kind == token_kind
  }

  /// Whether the parser has reached the end of the input.
  ///
  /// Will return `true` if the tokens vector provided is empty,
  /// or if the index is at the end of the tokens vector. In other words,
  /// when the last token is reached, it is considered that `EOF` has also
  /// been reached.
  pub fn is_at_last_token_or_past(&self) -> bool {
    // NOTE: The check for an empty token list is necessary,
    // otherwise it would result in Rust attempting to subtract
    // `1` from `0`, on an `usize` type, which will cause an underflow
    // panic.
    self.tokens.is_empty() || self.index >= self.tokens.len() - 1
  }

  fn is_path_segment(&self) -> bool {
    self.is(&lexer::TokenKind::ColonDouble) && !self.peek_is(&lexer::TokenKind::LessThan)
  }

  fn parse_path(&mut self, symbol_kind: symbol_table::SymbolKind) -> diagnostic::Maybe<ast::Path> {
    let first_segment = self.parse_name()?;

    let second_segment = if self.is_path_segment() {
      self.skip()?;

      Some(self.parse_name()?)
    } else {
      None
    };

    let is_absolute = self.is_path_segment();

    let base_name = if is_absolute {
      self.skip()?;

      self.parse_name()?
    } else {
      first_segment.clone()
    };

    // If the base name was parsed, and there's another double colon
    // afterwards, continue to parse the sub name.
    let sub_name = if is_absolute && self.is_path_segment() {
      self.skip()?;

      Some(self.parse_name()?)
    } else {
      second_segment.clone()
    };

    let qualifier = if is_absolute {
      Some(symbol_table::Qualifier {
        package_name: first_segment,
        // REVISE: Rewrite code to avoid using `unwrap`.
        module_name: second_segment.unwrap(),
      })
    } else {
      None
    };

    Ok(ast::Path {
      link_id: self.id_generator.next_link_id(),
      qualifier,
      base_name,
      sub_name,
      symbol_kind,
    })
  }

  fn parse_name(&mut self) -> diagnostic::Maybe<String> {
    // REVIEW: Illegal/unrecognized tokens MAY also be represented under 'Identifier'? Is this a problem? Maybe lexer logic bug?

    let name = match self.get_token()? {
      lexer::TokenKind::Identifier(value) => value.clone(),
      _ => return Err(self.expected("identifier")),
    };

    self.skip()?;

    Ok(name)
  }

  fn parse_statement(&mut self) -> diagnostic::Maybe<ast::Statement> {
    let statement = match self.get_token()? {
      lexer::TokenKind::Let => ast::Statement::Binding(std::rc::Rc::new(self.parse_binding()?)),
      lexer::TokenKind::Const => {
        ast::Statement::Constant(std::rc::Rc::new(self.parse_constant(true)?))
      }
      lexer::TokenKind::Write => {
        ast::Statement::PointerAssignment(std::rc::Rc::new(self.parse_pointer_assignment()?))
      }
      _ => ast::Statement::InlineExpr(self.parse_expr()?),
    };

    // Allow semi-colons to optionally appear at the end of statements. This
    // circumvents the edge case where there is ambiguity between two expressions
    // in different lines. For example, an expression might be on the first line,
    // then on the second one an array literal: this will be promoted to an indexing
    // expression.
    if self.is(&lexer::TokenKind::SemiColon) {
      self.skip()?;
    }

    Ok(statement)
  }

  /// %indent (%statement)+ %dedent
  fn parse_block(&mut self) -> diagnostic::Maybe<Block> {
    // CONSIDER: Instead of implicitly returning the last statement, have an optional keyword at the last statement be 'return' to indicate that a value was indeed returned. To avoid problems when there is a single statement, simply consider having a flag on whether the statement is returned or not, then consider this on type-sensitive operations during lowering or anywhere that the return value is used. There is a problem with this approach: all blocks would need to "return" their values, even those inside if-expressions! This would be too much. Perhaps special case function bodies? For example, a parameter could be passed to the "parse_block" parsing function (this), so that it knows when it's parsing a function block. This could be a good idea. Consider simply accepting a parameter here to indicate whether this block must use the return parameter to yield (ie. it is a function body).

    let mut statements = Vec::new();
    let mut last_statement_opt = None;

    self.skip_one(&lexer::TokenKind::Indent)?;

    loop {
      if let Some(previous_statement) = last_statement_opt {
        statements.push(std::rc::Rc::new(previous_statement));
      }

      last_statement_opt = Some(self.parse_statement()?);

      if self.is(&lexer::TokenKind::Dedent) {
        break;
      }
    }

    self.skip_one(&lexer::TokenKind::Dedent)?;

    let last_statement = match last_statement_opt {
      Some(statement) => statement,
      // This prevents parsing empty blocks with indents and dedents.
      // To specify an empty block, at least the `discard` keyword must be used.
      None => {
        return Err(vec![
          diagnostic::Diagnostic::BlocksMustHaveAtLeastOneStatement,
        ])
      }
    };

    // If the last statement is not a binding nor a constant, but an
    // expression then it qualifies as a yield value.
    let yield_value = if let ast::Statement::InlineExpr(inline_expr) = last_statement {
      inline_expr
    } else {
      ast::Expr::Statement(std::rc::Rc::new(last_statement))
    };

    Ok(Block {
      statements,
      type_id: self.id_generator.next_type_id(),
      yield_value,
    })
  }

  /// {nat8 | nat16 | nat | nat64 | int8 | int16 | int | int64 | real16 | real | real64}
  fn parse_number_type(&mut self) -> diagnostic::Maybe<types::PrimitiveType> {
    let current_token = self.get_token()?;

    let bit_width = match current_token {
      lexer::TokenKind::TypeInt8 | lexer::TokenKind::TypeNat8 => types::BitWidth::Width8,
      lexer::TokenKind::TypeInt16 | lexer::TokenKind::TypeNat16 | lexer::TokenKind::TypeReal16 => {
        types::BitWidth::Width16
      }
      lexer::TokenKind::TypeInt32 | lexer::TokenKind::TypeNat32 | lexer::TokenKind::TypeReal32 => {
        types::BitWidth::Width32
      }
      lexer::TokenKind::TypeInt64 | lexer::TokenKind::TypeNat64 | lexer::TokenKind::TypeReal64 => {
        types::BitWidth::Width64
      }
      _ => return Err(self.expected("number type")),
    };

    let is_real = matches!(
      current_token,
      lexer::TokenKind::TypeReal16 | lexer::TokenKind::TypeReal32 | lexer::TokenKind::TypeReal64
    );

    let is_signed = matches!(
      current_token,
      lexer::TokenKind::TypeInt8
        | lexer::TokenKind::TypeInt16
        | lexer::TokenKind::TypeInt32
        | lexer::TokenKind::TypeInt64
    );

    self.skip()?;

    Ok(if is_real {
      types::PrimitiveType::Real(bit_width)
    } else {
      types::PrimitiveType::Integer(bit_width, is_signed)
    })
  }

  /// bool
  fn parse_bool_type(&mut self) -> diagnostic::Maybe<types::PrimitiveType> {
    self.skip_one(&lexer::TokenKind::TypeBool)?;

    Ok(types::PrimitiveType::Bool)
  }

  fn parse_type(&mut self) -> diagnostic::Maybe<types::Type> {
    let ty = match self.get_token()? {
      lexer::TokenKind::Wildcard => {
        self.skip()?;

        // BUG: (test:type_infer) The reason this is causing problems is because on the `inference` module, when type variables are created, they are inserted against themselves. But here, they are only created, with no substitution specified.
        types::Type::Variable(types::TypeVariable {
          debug_name: "infer",
          substitution_id: symbol_table::SubstitutionId(self.id_generator.next()),
        })
      }
      lexer::TokenKind::Identifier(_) => types::Type::Stub(self.parse_stub_type()?),
      lexer::TokenKind::BraceL => types::Type::Object(self.parse_object_type()?),
      lexer::TokenKind::TypeUnit => self.parse_unit_type()?,
      lexer::TokenKind::TypeBool => types::Type::Primitive(self.parse_bool_type()?),
      lexer::TokenKind::TypeChar => {
        self.skip()?;

        types::Type::Primitive(types::PrimitiveType::Char)
      }
      lexer::TokenKind::ParenthesesL => self.parse_tuple_type()?,
      lexer::TokenKind::TypeInt8
      | lexer::TokenKind::TypeInt16
      | lexer::TokenKind::TypeInt32
      | lexer::TokenKind::TypeInt64
      | lexer::TokenKind::TypeNat8
      | lexer::TokenKind::TypeNat16
      | lexer::TokenKind::TypeNat32
      | lexer::TokenKind::TypeNat64
      | lexer::TokenKind::TypeReal16
      | lexer::TokenKind::TypeReal32
      | lexer::TokenKind::TypeReal64 => types::Type::Primitive(self.parse_number_type()?),
      lexer::TokenKind::Ampersand => {
        self.skip()?;

        types::Type::Reference(Box::new(self.parse_type()?))
      }
      lexer::TokenKind::TypeOpaque => {
        self.skip()?;

        types::Type::Opaque
      }
      lexer::TokenKind::Asterisk => {
        self.skip()?;

        self.parse_type()?.into_pointer_type()
      }
      lexer::TokenKind::TypeString => {
        self.skip()?;

        types::Type::Primitive(types::PrimitiveType::CString)
      }
      _ => return Err(self.expected("type")),
    };

    Ok(match ty {
      // Promote to a signature type, if applicable.
      types::Type::Tuple(tuple_type) if self.is(&lexer::TokenKind::Arrow) => {
        types::Type::Signature(self.parse_signature_type(tuple_type.0)?)
      }
      ty => ty,
    })
  }

  /// '{' (%name ':' %type ',')* '}'
  fn parse_object_type(&mut self) -> diagnostic::Maybe<types::ObjectType> {
    self.skip_one(&lexer::TokenKind::BraceL)?;

    let mut fields = types::ObjectFieldMap::new();
    const TERMINATOR: lexer::TokenKind = lexer::TokenKind::BraceR;

    while self.until_terminator(&TERMINATOR)? {
      let field_name = self.parse_name()?;

      if fields.contains_key(&field_name) {
        return Err(vec![diagnostic::Diagnostic::RepeatedObjectField(
          field_name,
        )]);
      }

      self.skip_one(&lexer::TokenKind::Colon)?;

      let ty = self.parse_type()?;

      fields.insert(field_name, ty);
      self.skip_comma(&TERMINATOR)?;
    }

    Self::check_llvm_size(fields.len())?;

    Ok(types::ObjectType {
      fields,
      kind: types::ObjectKind::Closed,
    })
  }

  /// unit
  fn parse_unit_type(&mut self) -> diagnostic::Maybe<types::Type> {
    self.skip_one(&lexer::TokenKind::TypeUnit)?;

    Ok(types::Type::Unit)
  }

  /// '(' (%type ',')* ')' '->' %type ('->' %type)*
  fn parse_signature_type(
    &mut self,
    parameter_types: Vec<types::Type>,
  ) -> diagnostic::Maybe<types::SignatureType> {
    self.skip_one(&lexer::TokenKind::Arrow)?;

    let return_type = Box::new(self.parse_type()?);

    Ok(types::SignatureType {
      parameter_types,
      return_type,
      // CONSIDER: Support for variadic functions types? Such as a reference to an foreign that is variadic?
      arity_mode: types::ArityMode::Fixed,
    })
  }

  /// %path (%generic_hints)?
  fn parse_stub_type(&mut self) -> diagnostic::Maybe<types::StubType> {
    let path = self.parse_path(symbol_table::SymbolKind::Type)?;

    let generic_hints = if self.is(&lexer::TokenKind::LessThan) {
      self.parse_generic_hints()?
    } else {
      Vec::default()
    };

    Ok(types::StubType {
      universe_id: self
        .id_generator
        .next_artifact_id(format!("stub_type.{}", path.base_name)),
      path,
      generic_hints,
    })
  }

  /// %name (':' %type)?
  fn parse_parameter(&mut self, position: LlvmSize) -> diagnostic::Maybe<ast::Parameter> {
    let name = self.parse_name()?;

    let type_hint = if self.is(&lexer::TokenKind::Colon) {
      self.skip()?;

      Some(self.parse_type()?)
    } else {
      None
    };

    Ok(ast::Parameter {
      name,
      type_hint,
      position,
      registry_id: self.id_generator.next_registry_id(),
      type_id: self.id_generator.next_type_id(),
    })
  }

  /// '(' {%parameter* (,)} (+) ')' '->' %type (uses (%string ',')+)?
  fn parse_signature(
    &mut self,
    invoker: SignatureParserInvoker,
  ) -> diagnostic::Maybe<ast::Signature> {
    self.skip_one(&lexer::TokenKind::ParenthesesL)?;

    let mut parameters = Vec::new();
    let mut is_variadic = false;

    const TERMINATOR: lexer::TokenKind = lexer::TokenKind::ParenthesesR;

    while self.until_terminator(&TERMINATOR)? {
      if self.is(&lexer::TokenKind::EllipsisLong) {
        self.skip()?;
        self.skip_one(&TERMINATOR)?;
        is_variadic = true;

        // Variadic indicator must be the last token in the signature.
        break;
      }

      let parameter = self.parse_parameter(Self::get_llvm_size(parameters.len())?)?;

      // Since polytypes will not be supported, type hints for
      // function parameters are required.
      if parameter.type_hint.is_none() {
        return Err(vec![diagnostic::Diagnostic::ParameterTypeHintRequired(
          parameter.name,
        )]);
      }

      parameters.push(std::rc::Rc::new(parameter));

      self.skip_comma(&TERMINATOR)?;
    }

    Self::check_llvm_size(parameters.len())?;

    // Return type hints are required for functions for the
    // sake of clarity and explicitness.
    let return_type_hint = if self.is(&lexer::TokenKind::Arrow) {
      self.skip()?;

      Some(self.parse_type()?)
    } else {
      return Err(vec![diagnostic::Diagnostic::ReturnTypeHintRequired]);
    };

    let kind = match invoker {
      SignatureParserInvoker::ForeignFunction => ast::SignatureKind::ForeignFunction,
      SignatureParserInvoker::Function => ast::SignatureKind::Function,
      SignatureParserInvoker::Effect => ast::SignatureKind::Effect,
    };

    let effects_used = if self.is(&lexer::TokenKind::Uses) {
      self.skip()?;

      let mut effects_used = vec![self.parse_name()?];

      while self.is(&lexer::TokenKind::Comma) {
        self.skip()?;

        effects_used.push(self.parse_name()?);
      }

      effects_used
    } else {
      Vec::default()
    };

    Ok(ast::Signature {
      parameters,
      return_type_hint,
      is_variadic,
      return_type_id: self.id_generator.next_type_id(),
      kind,
      effects_used,
    })
  }

  /// func %name (%generics)? %signature %block
  fn parse_function(&mut self) -> diagnostic::Maybe<ast::Function> {
    self.skip_one(&lexer::TokenKind::Func)?;

    let name = self.parse_name()?;

    let generics = if self.is(&lexer::TokenKind::LessThan) {
      self.parse_generics()?
    } else {
      ast::Generics::default()
    };

    let signature = self.parse_signature(SignatureParserInvoker::Function)?;

    self.skip_one(&lexer::TokenKind::Colon)?;

    let body = std::rc::Rc::new(self.parse_block()?);

    Ok(ast::Function {
      name,
      signature: std::rc::Rc::new(signature),
      body,
      registry_id: self.id_generator.next_registry_id(),
      generics,
      type_id: self.id_generator.next_type_id(),
    })
  }

  /// func %name %signature
  fn parse_foreign_function(&mut self) -> diagnostic::Maybe<ast::ForeignFunction> {
    self.skip_one(&lexer::TokenKind::Func)?;

    let name = self.parse_name()?;
    let signature = self.parse_signature(SignatureParserInvoker::ForeignFunction)?;

    Ok(ast::ForeignFunction {
      name,
      signature: std::rc::Rc::new(signature),
      registry_id: self.id_generator.next_registry_id(),
      type_id: self.id_generator.next_type_id(),
    })
  }

  /// var %name ':' %type
  fn parse_foreign_var(&mut self) -> diagnostic::Maybe<ast::ForeignStatic> {
    self.skip_one(&lexer::TokenKind::Var)?;

    let name = self.parse_name()?;

    self.skip_one(&lexer::TokenKind::Colon)?;

    let ty = self.parse_type()?;

    Ok(ast::ForeignStatic {
      name,
      ty,
      registry_id: self.id_generator.next_registry_id(),
    })
  }

  /// foreign ':' %indent {%foreign_function | %foreign_var}* %dedent
  fn parse_foreign_cluster(&mut self) -> diagnostic::Maybe<ast::ForeignCluster> {
    self.skip_many(&[
      lexer::TokenKind::Foreign,
      lexer::TokenKind::Colon,
      lexer::TokenKind::Indent,
    ])?;

    let mut foreign_functions_or_vars = Vec::new();

    // FIXME: Require at least one item, but using a while loop! (Previously it was a do-while loop, in the form of a `loop`).
    while self.until_terminator(&lexer::TokenKind::Dedent)? {
      let node = match self.get_token()? {
        lexer::TokenKind::Func => {
          ast::Item::ForeignFunction(std::rc::Rc::new(self.parse_foreign_function()?))
        }
        lexer::TokenKind::Var => ast::Item::ForeignVar(std::rc::Rc::new(self.parse_foreign_var()?)),
        _ => return Err(self.expected("foreign function or static")),
      };

      foreign_functions_or_vars.push(node);
    }

    Ok(ast::ForeignCluster {
      foreigns: foreign_functions_or_vars,
    })
  }

  /// {%function | %foreign_function | %foreign_var | %type_def | %enum | %type_class}
  fn parse_item(&mut self) -> diagnostic::Maybe<ast::Item> {
    // CONSIDER: Why not move this check into the `get()` method?
    if self.is_at_last_token_or_past() {
      return Err(self.expected("top-level construct"));
    }

    let token = self.get_token()?;

    // TODO: In the future, type aliases, constants, enums, and effects should all be able to be defined under any kind of scope, not just as a top-level node.
    Ok(match token {
      lexer::TokenKind::Func => ast::Item::Function(std::rc::Rc::new(self.parse_function()?)),
      lexer::TokenKind::Foreign => {
        ast::Item::ForeignCluster(std::rc::Rc::new(self.parse_foreign_cluster()?))
      }
      lexer::TokenKind::Enum => ast::Item::Union(std::rc::Rc::new(self.parse_union()?)),
      lexer::TokenKind::Type => ast::Item::TypeDef(std::rc::Rc::new(self.parse_type_def()?)),
      lexer::TokenKind::Import => ast::Item::Import(std::rc::Rc::new(self.parse_import()?)),
      lexer::TokenKind::Effect => ast::Item::Effect(std::rc::Rc::new(self.parse_effect()?)),
      lexer::TokenKind::Const => ast::Item::Constant(std::rc::Rc::new(self.parse_constant(false)?)),
      _ => return Err(self.expected("top-level construct")),
    })
  }

  fn parse_with(&mut self, object: ast::Expr) -> diagnostic::Maybe<ast::With> {
    self.skip_one(&lexer::TokenKind::With)?;

    let deltas = self.parse_object()?;

    Ok(ast::With { object, deltas })
  }

  /// const %name ':' %type '=' %expr
  fn parse_constant(&mut self, is_local: bool) -> diagnostic::Maybe<ast::Constant> {
    self.skip_one(&lexer::TokenKind::Const)?;

    let name = self.parse_name()?;

    self.skip_one(&lexer::TokenKind::Colon)?;

    let ty = self.parse_type()?;

    self.skip_one(&lexer::TokenKind::Equal)?;

    let value = Box::new(self.parse_expr()?);

    Ok(ast::Constant {
      was_declared_locally: is_local,
      registry_id: self.id_generator.next_registry_id(),
      name,
      ty,
      value,
    })
  }

  /// type %name (%generics)? = %type
  fn parse_type_def(&mut self) -> diagnostic::Maybe<ast::TypeDef> {
    self.skip_one(&lexer::TokenKind::Type)?;

    let name = self.parse_name()?;

    let generics = if self.is(&lexer::TokenKind::LessThan) {
      self.parse_generics()?
    } else {
      ast::Generics::default()
    };

    self.skip_one(&lexer::TokenKind::Equal)?;

    let body = self.parse_type()?;

    Ok(ast::TypeDef {
      registry_id: self.id_generator.next_registry_id(),
      name,
      body,
      generics,
    })
  }

  /// let %name (':' %type) {'=', '=>', ':='} %expr
  fn parse_binding(&mut self) -> diagnostic::Maybe<ast::Binding> {
    self.skip_one(&lexer::TokenKind::Let)?;

    let name = self.parse_name()?;

    let type_hint = if self.is(&lexer::TokenKind::Colon) {
      self.skip()?;

      Some(self.parse_type()?)
    } else {
      None
    };

    self.skip()?;

    let value = self.parse_expr()?;

    Ok(ast::Binding {
      registry_id: self.id_generator.next_registry_id(),
      type_id: self.id_generator.next_type_id(),
      name,
      value,
      type_hint,
    })
  }

  /// if %expr ':' %expr (elif %expr ':' %expr)* (else ':' %expr)? end
  fn parse_if(&mut self) -> diagnostic::Maybe<ast::If> {
    self.skip_one(&lexer::TokenKind::If)?;

    let condition = self.parse_expr()?;

    self.skip_one(&lexer::TokenKind::Colon)?;

    let then_branch = self.parse_expr()?;
    let mut elif_branches = Vec::new();

    while self.is(&lexer::TokenKind::Elif) {
      self.skip()?;

      let elif_condition = self.parse_expr()?;

      self.skip_one(&lexer::TokenKind::Colon)?;

      let elif_value = self.parse_expr()?;

      elif_branches.push((elif_condition, elif_value));
    }

    let else_branch = if self.is(&lexer::TokenKind::Else) {
      self.skip()?;
      self.skip_one(&lexer::TokenKind::Colon)?;

      Some(self.parse_expr()?)
    } else {
      None
    };

    Ok(ast::If {
      type_id: self.id_generator.next_type_id(),
      condition,
      then_branch,
      elif_branches,
      else_branch,
    })
  }

  // unsafe ':' %expr
  fn parse_unsafe(&mut self) -> diagnostic::Maybe<ast::Unsafe> {
    self.skip_many(&[lexer::TokenKind::Unsafe, lexer::TokenKind::Colon])?;

    Ok(ast::Unsafe(self.parse_expr()?))
  }

  /// {true | false}
  fn parse_bool_literal(&mut self) -> diagnostic::Maybe<ast::LiteralKind> {
    // OPTIMIZE: There shouldn't be a need to clone the token here.
    match self.get_token()?.to_owned() {
      lexer::TokenKind::Bool(value) => {
        self.skip()?;

        Ok(ast::LiteralKind::Bool(value))
      }
      _ => Err(self.expected("boolean literal")),
    }
  }

  /// 0-9+ ('::' %type)?
  fn parse_number_literal(&mut self) -> diagnostic::Maybe<ast::LiteralKind> {
    let (value, is_real) = match self.get_token()? {
      lexer::TokenKind::Number(value, is_real) => (*value, *is_real),
      _ => return Err(self.expected("numeric literal")),
    };

    self.skip()?;

    let type_hint = if self.is(&lexer::TokenKind::ColonDouble) {
      self.skip()?;

      Some(self.parse_type()?)
    } else {
      None
    };

    let bit_width = if let Some(type_hint) = &type_hint {
      match &type_hint {
        types::Type::Primitive(types::PrimitiveType::Integer(bit_width, _)) => *bit_width,
        types::Type::Primitive(types::PrimitiveType::Real(bit_width)) => *bit_width,
        _ => return Err(self.expected("numeric type")),
      }
    } else {
      let minimum_bit_width = Self::minimum_bit_width_of(&value)
        .map_err(|_| vec![diagnostic::Diagnostic::NumberLiteralTooBig])?;

      if minimum_bit_width < types::BitWidth::Width32 {
        // Default size to a minimum 32 bit-width.
        types::BitWidth::Width32
      } else {
        minimum_bit_width
      }
    };

    Ok(ast::LiteralKind::Number {
      is_real,
      type_hint,
      bit_width,
      value,
    })
  }

  /// '"' [^"]* '"'
  fn parse_string_literal(&mut self) -> diagnostic::Maybe<ast::LiteralKind> {
    let result = match self.get_token()? {
      lexer::TokenKind::String(value) => ast::LiteralKind::String(value.clone()),
      _ => return Err(self.expected("string literal")),
    };

    self.skip()?;

    Ok(result)
  }

  /// null ('::' %type)?
  fn parse_null_literal(&mut self) -> diagnostic::Maybe<ast::LiteralKind> {
    self.skip_one(&lexer::TokenKind::Null)?;

    // Although the type can be inferred from a binding, in the
    // case that the null value is used as an expression somewhere,
    // the user may want to qualify it to fill in or aid type inference.
    let type_hint_opt = if self.is(&lexer::TokenKind::ColonDouble) {
      self.skip()?;

      Some(self.parse_type()?)
    } else {
      None
    };

    Ok(ast::LiteralKind::Nullptr(type_hint_opt))
  }

  // TODO: Still need to implement other utilities for working with memory, such as alignof.

  /// sizeof '::' '<' %type '>'
  fn parse_sizeof(&mut self) -> diagnostic::Maybe<ast::Sizeof> {
    self.skip_many(&[
      lexer::TokenKind::Sizeof,
      lexer::TokenKind::ColonDouble,
      lexer::TokenKind::LessThan,
    ])?;

    let ty = self.parse_type()?;

    self.skip_one(&lexer::TokenKind::GreaterThan)?;

    Ok(ast::Sizeof {
      type_id: self.id_generator.next_type_id(),
      ty,
    })
  }

  fn parse_literal(&mut self) -> diagnostic::Maybe<ast::Literal> {
    let kind = match self.get_token()? {
      lexer::TokenKind::Bool(_) => self.parse_bool_literal()?,
      lexer::TokenKind::Number(..) => self.parse_number_literal()?,
      lexer::TokenKind::Char(_) => self.parse_char_literal()?,
      lexer::TokenKind::String(_) => self.parse_string_literal()?,
      lexer::TokenKind::Null => self.parse_null_literal()?,
      _ => return Err(self.expected("literal")),
    };

    Ok(ast::Literal {
      type_id: self.id_generator.next_type_id(),
      kind,
    })
  }

  fn parse_char_literal(&mut self) -> diagnostic::Maybe<ast::LiteralKind> {
    let result = match self.get_token()? {
      lexer::TokenKind::Char(value) => ast::LiteralKind::Char(value.to_owned()),
      _ => return Err(self.expected("character literal")),
    };

    self.skip()?;

    Ok(result)
  }

  fn is_promotion_chain(&self) -> bool {
    let current_token_kind = match self.get_token() {
      Ok(token_kind) => token_kind,
      Err(..) => return false,
    };

    matches!(
      current_token_kind,
      // Object access.
      lexer::TokenKind::Dot
        // Call site without generic hints.
        | lexer::TokenKind::ParenthesesL
        // Call site with generic hints.
        | lexer::TokenKind::ColonDouble
        // Pointer indexing.
        | lexer::TokenKind::BracketL
        | lexer::TokenKind::As
        | lexer::TokenKind::Pipe
        | lexer::TokenKind::With
    )
  }

  /// Attempt to promote the given item if applicable.
  ///
  /// If after an expression there is an applicable token that may change the
  /// meaning of the overall expression, it will be *promoted*. For example,
  /// if a period is present after an expression, the expression is upgraded to
  /// an object access item.
  fn try_promote(&mut self, mut expr: ast::Expr) -> diagnostic::Maybe<ast::Expr> {
    // BUG: Things that are on the next line may be parsed as part of the promotion chain. This also seems to be the case for JavaScript. Perhaps this could be overlooked? If so, it needs to be documented.
    // Promote the item to a chain, if applicable.
    while self.is_promotion_chain() {
      expr = match self.get_token()? {
        lexer::TokenKind::Pipe => ast::Expr::CallSite(std::rc::Rc::new(self.parse_pipe(expr)?)),
        lexer::TokenKind::ColonDouble if self.peek_is(&lexer::TokenKind::LessThan) => {
          ast::Expr::CallSite(std::rc::Rc::new(self.parse_call_site(expr)?))
        }
        lexer::TokenKind::ParenthesesL => {
          ast::Expr::CallSite(std::rc::Rc::new(self.parse_call_site(expr)?))
        }
        lexer::TokenKind::ColonDouble => ast::Expr::Cast(std::rc::Rc::new(self.parse_cast(expr)?)),
        lexer::TokenKind::As => ast::Expr::Cast(std::rc::Rc::new(self.parse_cast(expr)?)),
        lexer::TokenKind::Dot if matches!(self.peek(), Some(lexer::TokenKind::Number(..))) => {
          ast::Expr::TupleIndexing(std::rc::Rc::new(self.parse_tuple_indexing(expr)?))
        }
        lexer::TokenKind::Dot if self.peek_is(&lexer::TokenKind::As) => {
          ast::Expr::Cast(std::rc::Rc::new(self.parse_cast(expr)?))
        }
        lexer::TokenKind::Dot => {
          ast::Expr::ObjectAccess(std::rc::Rc::new(self.parse_object_access(expr)?))
        }
        lexer::TokenKind::BracketL => {
          ast::Expr::PointerIndexing(std::rc::Rc::new(self.parse_pointer_indexing(expr)?))
        }
        lexer::TokenKind::With => ast::Expr::With(std::rc::Rc::new(self.parse_with(expr)?)),
        _ => unreachable!("all tokens that indicate promotion should have been handled"),
      };
    }

    Ok(expr)
  }

  fn parse_primary_expr(&mut self) -> diagnostic::Maybe<ast::Expr> {
    let mut expr = match self.get_token()? {
      lexer::TokenKind::VerticalBar | lexer::TokenKind::At => {
        ast::Expr::Closure(std::rc::Rc::new(self.parse_closure()?))
      }
      lexer::TokenKind::Discard => ast::Expr::Discard(std::rc::Rc::new(self.parse_discard()?)),
      lexer::TokenKind::If => ast::Expr::If(std::rc::Rc::new(self.parse_if()?)),
      lexer::TokenKind::BraceL => ast::Expr::Object(std::rc::Rc::new(self.parse_object()?)),
      lexer::TokenKind::Indent => ast::Expr::Block(std::rc::Rc::new(self.parse_block()?)),
      lexer::TokenKind::Unsafe => ast::Expr::Unsafe(std::rc::Rc::new(self.parse_unsafe()?)),
      lexer::TokenKind::Sizeof => ast::Expr::Sizeof(std::rc::Rc::new(self.parse_sizeof()?)),
      lexer::TokenKind::Match => ast::Expr::Match(std::rc::Rc::new(self.parse_match()?)),
      lexer::TokenKind::Try => ast::Expr::Try(std::rc::Rc::new(self.parse_try()?)),
      lexer::TokenKind::Resume => ast::Expr::Resume(std::rc::Rc::new(self.parse_resume()?)),
      lexer::TokenKind::Identifier(_) => {
        ast::Expr::Reference(std::rc::Rc::new(self.parse_reference()?))
      }
      lexer::TokenKind::Pass => {
        self.skip()?;

        ast::Expr::Pass(std::rc::Rc::new(ast::Pass))
      }
      // Parentheses requires disambiguation between tuple or group.
      lexer::TokenKind::ParenthesesL => {
        self.skip()?;

        // REVISE: Decouple logic. Perhaps this shouldn't be here (checking for empty tuple).
        if self.is(&lexer::TokenKind::ParenthesesR) {
          self.skip()?;

          ast::Expr::Tuple(std::rc::Rc::new(self.parse_tuple(None)?))
        } else {
          let expr = self.parse_expr()?;

          if self.is(&lexer::TokenKind::Comma) {
            ast::Expr::Tuple(std::rc::Rc::new(self.parse_tuple(Some(expr))?))
          } else {
            ast::Expr::Group(std::rc::Rc::new(self.parse_group(expr)?))
          }
        }
      }
      _ if self.is_unary_operator_token() => {
        ast::Expr::UnaryOp(std::rc::Rc::new(self.parse_unary_op()?))
      }
      // Default to a literal if nothing else matched.
      _ => ast::Expr::Literal(self.parse_literal()?),
    };

    // TODO: Temporary positioning for now. Later on, find a more flexible alternative.
    if self.is(&lexer::TokenKind::Bang) {
      if let ast::Expr::Reference(reference) = &expr {
        expr = ast::Expr::UnionInstance(std::rc::Rc::new(
          self.parse_union_instance(reference.path.to_owned())?,
        ));
      }
    }

    self.try_promote(expr)
  }

  /// discard %expr
  fn parse_discard(&mut self) -> diagnostic::Maybe<ast::Discard> {
    self.skip_one(&lexer::TokenKind::Discard)?;

    let value = self.parse_expr()?;

    Ok(ast::Discard(value))
  }

  fn parse_tuple_indexing(
    &mut self,
    indexed_tuple: ast::Expr,
  ) -> diagnostic::Maybe<ast::TupleIndex> {
    self.skip_one(&lexer::TokenKind::Dot)?;

    // REVIEW: What about a dynamic index? Should there be support for that? But then we'd have to also deal with out-of-bounds possibility for this. Need a system to handle out-of-bounds indexing.
    let index = match self.get_token() {
      // FIXME: Wrong cast (truncate). Need better way to obtain the index value.
      Ok(lexer::TokenKind::Number(value, _)) => *value as u32,
      _ => return Err(self.expected("number")),
    };

    Ok(ast::TupleIndex {
      index,
      indexed_tuple,
      indexed_tuple_type_id: self.id_generator.next_type_id(),
      type_id: self.id_generator.next_type_id(),
    })
  }

  fn parse_union_instance(&mut self, path: ast::Path) -> diagnostic::Maybe<ast::UnionInstance> {
    // TODO: Temporary syntax requirement for aiding the parser.
    self.skip_one(&lexer::TokenKind::Bang)?;

    let value = if self.is(&lexer::TokenKind::ParenthesesL) {
      self.skip()?;

      let value = ast::UnionInstanceValue::Value(self.parse_expr()?);

      self.skip_one(&lexer::TokenKind::ParenthesesR)?;

      value
    } else {
      // TODO: Might need to merge string and numeric values under singleton.
      let name = self.parse_name()?;

      ast::UnionInstanceValue::Singleton(name)
    };

    Ok(ast::UnionInstance { path, value })
  }

  /// %expr '.' %name
  fn parse_object_access(&mut self, base_expr: ast::Expr) -> diagnostic::Maybe<ast::ObjectAccess> {
    self.skip_one(&lexer::TokenKind::Dot)?;

    Ok(ast::ObjectAccess {
      object: base_expr,
      type_id: self.id_generator.next_type_id(),
      base_expr_type_id: self.id_generator.next_type_id(),
      field_name: self.parse_name()?,
    })
  }

  /// {'+' | '-' | '*' | '/' | and | or | nand | nor | xor | '<' | '>' | '==' | '<=' | '>=' | '!=' | '%'}
  fn parse_binary_operator(&mut self) -> diagnostic::Maybe<ast::BinaryOperator> {
    let operator = match self.get_token()? {
      lexer::TokenKind::And => ast::BinaryOperator::And,
      lexer::TokenKind::Or => ast::BinaryOperator::Or,
      lexer::TokenKind::Nand => ast::BinaryOperator::Nand,
      lexer::TokenKind::Nor => ast::BinaryOperator::Nor,
      lexer::TokenKind::Xor => ast::BinaryOperator::Xor,
      lexer::TokenKind::Plus => ast::BinaryOperator::Add,
      lexer::TokenKind::Minus => ast::BinaryOperator::Subtract,
      lexer::TokenKind::Asterisk => ast::BinaryOperator::Multiply,
      lexer::TokenKind::Slash => ast::BinaryOperator::Divide,
      lexer::TokenKind::LessThan => ast::BinaryOperator::LessThan,
      lexer::TokenKind::GreaterThan => ast::BinaryOperator::GreaterThan,
      lexer::TokenKind::Equality => ast::BinaryOperator::Equality,
      lexer::TokenKind::LessThanEqualTo => ast::BinaryOperator::LessThanOrEqual,
      lexer::TokenKind::GreaterThanEqualTo => ast::BinaryOperator::GreaterThanOrEqual,
      lexer::TokenKind::Inequality => ast::BinaryOperator::Inequality,
      lexer::TokenKind::PercentSign => ast::BinaryOperator::Modulo,
      _ => return Err(self.expected("binary operator")),
    };

    self.skip()?;

    Ok(operator)
  }

  /// {not | '-' | '&' | '*'}
  fn parse_unary_operator(&mut self) -> diagnostic::Maybe<ast::UnaryOperator> {
    let operator = match self.get_token()? {
      lexer::TokenKind::Not => ast::UnaryOperator::Not,
      lexer::TokenKind::Ampersand => ast::UnaryOperator::ReferenceOf,
      lexer::TokenKind::Minus => ast::UnaryOperator::Negate,
      lexer::TokenKind::Asterisk => ast::UnaryOperator::Dereference,
      _ => return Err(self.expected("unary operator")),
    };

    self.skip()?;

    Ok(operator)
  }

  /// %expr %operator %expr
  fn parse_binary_op_or_default(
    &mut self,
    left_operand: ast::Expr,
    min_precedence: usize,
  ) -> diagnostic::Maybe<ast::Expr> {
    // CONSIDER: Moving to use the Pratt-parsing technique instead to replace the non-tail recursive method. Or, simply adjust this implementation to not be recursive.
    // SAFETY: This function needs revision for possible logic bugs.
    // REVISE: Expand function a little to improve readability.

    let mut current_token = if let Ok(token) = self.get_token() {
      token.to_owned()
    } else {
      return Ok(left_operand);
    };

    let precedence = Self::precedence_of(&current_token);
    let mut buffer = left_operand;

    // TODO: Document what is happening step by step, and check for bugs.
    while Parser::is_binary_operator_token(&current_token) && (precedence > min_precedence) {
      let operator = self.parse_binary_operator()?;
      let mut right_operand = self.parse_expr()?;

      current_token = self.get_token()?.to_owned();

      while Parser::is_binary_operator_token(&current_token)
        && Self::precedence_of(&current_token) > precedence
      {
        // REVIEW: Are we adding the correct amount of precedence here? Shouldn't there be a higher difference in precedence?
        // OPTIMIZE: Find a way to make this tail-recursive.
        right_operand = self.parse_binary_op_or_default(right_operand, precedence + 1)?;
        current_token = self.get_token()?.to_owned();
      }

      let kind = ast::Expr::BinaryOp(std::rc::Rc::new(ast::BinaryOp {
        type_id: self.id_generator.next_type_id(),
        left_operand: buffer,
        operand_type_id: self.id_generator.next_type_id(),
        right_operand,
        operator,
      }));

      buffer = kind;
    }

    Ok(buffer)
  }

  /// %expr {'.' as '<' %type '>' | as %type}
  fn parse_cast(&mut self, operand: ast::Expr) -> diagnostic::Maybe<ast::Cast> {
    let is_chain_style = if self.is(&lexer::TokenKind::Dot) {
      self.skip_one(&lexer::TokenKind::Dot)?;
      self.skip_one(&lexer::TokenKind::As)?;
      self.skip_one(&lexer::TokenKind::LessThan)?;

      true
    } else {
      self.skip_one(&lexer::TokenKind::As)?;

      false
    };

    let cast_type = self.parse_type()?;

    if is_chain_style {
      self.skip_one(&lexer::TokenKind::GreaterThan)?;
    }

    Ok(ast::Cast {
      type_id: self.id_generator.next_type_id(),
      operand_type_id: self.id_generator.next_type_id(),
      cast_type,
      operand,
    })
  }

  /// %operator %expr
  fn parse_unary_op(&mut self) -> diagnostic::Maybe<ast::UnaryOp> {
    let operator = self.parse_unary_operator()?;
    let operand = self.parse_expr()?;

    Ok(ast::UnaryOp {
      type_id: self.id_generator.next_type_id(),
      operand_type_id: self.id_generator.next_type_id(),
      operator,
      operand,
    })
  }

  fn parse_expr(&mut self) -> diagnostic::Maybe<ast::Expr> {
    let initial_expr = self.parse_primary_expr()?;

    // REVIEW: Should the precedence be zero here? If so, explain why. Is it because it's the initial expression?
    Ok(self.parse_binary_op_or_default(initial_expr, 0)?)
  }

  /// %expr %generic_hints '(' (%expr (','))* ')'
  fn parse_call_site(&mut self, callee: ast::Expr) -> diagnostic::Maybe<ast::CallSite> {
    let generic_hints = if self.is(&lexer::TokenKind::ColonDouble) {
      self.skip()?;

      self.parse_generic_hints()?
    } else {
      Vec::default()
    };

    self.skip_one(&lexer::TokenKind::ParenthesesL)?;

    let mut arguments = Vec::new();
    const TERMINATOR: lexer::TokenKind = lexer::TokenKind::ParenthesesR;

    while self.until_terminator(&TERMINATOR)? {
      arguments.push(ast::CallSiteArg {
        type_id: self.id_generator.next_type_id(),
        value: self.parse_expr()?,
      });

      self.skip_comma(&TERMINATOR)?;
    }

    let debug_name = if let Some(debug_name) = callee.find_debug_name() {
      format!("call_site.{}", debug_name)
    } else {
      String::from("call_site")
    };

    Ok(ast::CallSite {
      universe_id: self.id_generator.next_artifact_id(debug_name),
      type_id: self.id_generator.next_type_id(),
      callee_type_id: self.id_generator.next_type_id(),
      registry_id: self.id_generator.next_registry_id(),
      callee_expr: callee,
      arguments,
      generic_hints,
    })
  }

  /// '|>' %expr
  fn parse_pipe(&mut self, argument: ast::Expr) -> diagnostic::Maybe<ast::CallSite> {
    self.skip_one(&lexer::TokenKind::Pipe)?;

    let callee = self.parse_expr()?;

    Ok(ast::CallSite {
      universe_id: self.id_generator.next_artifact_id(String::from("pipe")),
      callee_expr: callee,
      type_id: self.id_generator.next_type_id(),
      callee_type_id: self.id_generator.next_type_id(),
      generic_hints: Vec::default(),
      arguments: vec![ast::CallSiteArg {
        type_id: self.id_generator.next_type_id(),
        value: argument,
      }],
      registry_id: self.id_generator.next_registry_id(),
    })
  }

  /// %path
  fn parse_reference(&mut self) -> diagnostic::Maybe<ast::Reference> {
    // REVIEW: Would there be an instance where this method can accept which symbol kind to parse?
    let path = self.parse_path(symbol_table::SymbolKind::Declaration)?;

    Ok(ast::Reference {
      type_id: self.id_generator.next_type_id(),
      path,
    })
  }

  /// enum %name %indent (%variant ',')+ %dedent
  fn parse_union(&mut self) -> diagnostic::Maybe<ast::Union> {
    self.skip_one(&lexer::TokenKind::Enum)?;

    let name = self.parse_name()?;

    self.skip_many(&[lexer::TokenKind::Colon, lexer::TokenKind::Indent])?;

    let mut variants = std::collections::BTreeMap::new();
    let union_registry_id = self.id_generator.next_registry_id();
    let mut singleton_index_counter = 0;

    // REVISE: Break apart into a separate `parse_union_variant` method, pass in the `union_id` and `singleton_index_counter` as arguments.
    loop {
      let name = self.parse_name()?;

      // TODO: Support for explicitly initialized values (strings and numbers).
      let kind = match self.get_token()? {
        lexer::TokenKind::ParenthesesL => ast::UnionVariantKind::Type(self.parse_type()?),
        // Default to a singleton.
        _ => {
          // let index = singleton_index_counter;

          // FIXME: Not tightly enforced. This is temporary; left for later. This parsing function is incomplete.
          if self.is(&lexer::TokenKind::Equal) {
            self.skip()?;
          }

          let result = if let Ok(lexer::TokenKind::Number(value, _)) = self.get_token().to_owned() {
            // TODO: Singletons' values should be `f64` instead, to allow storage for floating-point values?
            // NOTE: This conversion is safe; the fractional component
            // is simply dropped and ignored.
            let value = *value as i64;

            ast::UnionVariantKind::Singleton {
              name: self.parse_name()?,
              relative_index: singleton_index_counter,
              explicit_value: Some(value),
            }
          } else if let Ok(lexer::TokenKind::String(string)) = self.get_token() {
            ast::UnionVariantKind::String(string.to_owned())
          } else {
            ast::UnionVariantKind::Singleton {
              name: self.parse_name()?,
              relative_index: singleton_index_counter,
              explicit_value: None,
            }
          };

          singleton_index_counter += 1;

          result
        }
      };

      variants.insert(
        name.clone(),
        std::rc::Rc::new(ast::UnionVariant {
          union_id: union_registry_id.clone(),
          registry_id: self.id_generator.next_registry_id(),
          name,
          kind,
        }),
      );

      if self.is(&lexer::TokenKind::Comma) {
        self.skip()?;
      } else if self.is(&lexer::TokenKind::Dedent) {
        break;
      }
    }

    self.skip_one(&lexer::TokenKind::Dedent)?;
    Self::check_llvm_size(variants.len())?;

    Ok(ast::Union {
      registry_id: union_registry_id,
      name,
      variants,
    })
  }

  /// '{' (%name ':' %expr ',')+ '}'
  fn parse_object(&mut self) -> diagnostic::Maybe<ast::Object> {
    self.skip_one(&lexer::TokenKind::BraceL)?;

    let mut fields = std::collections::HashMap::new();

    loop {
      let field_name = self.parse_name()?;

      if fields.contains_key(&field_name) {
        return Err(vec![diagnostic::Diagnostic::RepeatedObjectField(
          field_name,
        )]);
      }

      let field_value = if self.is(&lexer::TokenKind::Colon) {
        self.skip_one(&lexer::TokenKind::Colon)?;

        self.parse_expr()?
      }
      // Sugar for referencing declarations with the same name as
      // the field name (short syntax).
      else {
        let reference = ast::Reference {
          type_id: self.id_generator.next_type_id(),
          path: ast::Path {
            base_name: field_name.clone(),
            link_id: self.id_generator.next_link_id(),
            qualifier: None,
            sub_name: None,
            symbol_kind: symbol_table::SymbolKind::Declaration,
          },
        };

        ast::Expr::Reference(std::rc::Rc::new(reference))
      };

      fields.insert(field_name, field_value);

      // NOTE: If the current token is not a right brace, then a comma will
      // be forcefully consumed. This removes the posibility of an infinite
      // loop upon reaching `EOF` without ever reaching a right brace token.
      if self.is(&lexer::TokenKind::BraceR) {
        break;
      } else {
        self.skip_one(&lexer::TokenKind::Comma)?;
      }
    }

    // Skip the closing brace symbol.
    self.skip_one(&lexer::TokenKind::BraceR)?;

    Ok(ast::Object {
      fields,
      type_id: self.id_generator.next_type_id(),
    })
  }

  fn parse_closure(&mut self) -> diagnostic::Maybe<ast::Closure> {
    // TODO: Support for no parentheses when parsed as an effect handler.
    // CONSIDER: Adding a `ClosureKind` field to closure, to specify that it is an effect handler. This will be useful during lowering, as effect handler closures might need slightly different logic, or be more restrictive.

    let mut captures = Vec::new();
    let registry_id = self.id_generator.next_registry_id();
    let mut parameters = Vec::new();
    let mut is_parsing_captures = true;
    let mut index = 0;

    const ENCLOSURE: lexer::TokenKind = lexer::TokenKind::VerticalBar;

    self.skip_one(&ENCLOSURE)?;

    while self.until_terminator(&ENCLOSURE)? {
      if self.is(&lexer::TokenKind::At) {
        if !is_parsing_captures {
          return Err(vec![diagnostic::Diagnostic::ClosureCaptureAfterParameters]);
        }

        self.skip()?;

        captures.push(ast::ClosureCapture {
          name: self.parse_name()?,
          closure_registry_id: registry_id,
          target_link_id: self.id_generator.next_link_id(),
          type_id: self.id_generator.next_type_id(),
          registry_id: self.id_generator.next_registry_id(),
          index: Self::get_llvm_size(index)?,
        });
      } else {
        is_parsing_captures = false;

        let parameter = self.parse_parameter(Self::get_llvm_size(parameters.len())?)?;

        // NOTE: Closure signatures are allowed to skip parameter types.

        parameters.push(std::rc::Rc::new(parameter));
      }

      index += 1;
      self.skip_comma(&ENCLOSURE)?;
    }

    Self::check_llvm_size(parameters.len())?;

    let signature = ast::Signature {
      effects_used: Vec::default(),
      is_variadic: false,
      kind: ast::SignatureKind::Closure,
      parameters,
      return_type_hint: None,
      return_type_id: self.id_generator.next_type_id(),
    };

    let body = self.parse_expr()?;

    Ok(ast::Closure {
      captures,
      signature: std::rc::Rc::new(signature),
      body,
      registry_id,
      type_id: self.id_generator.next_type_id(),
    })
  }

  /// '(' %expr ')'
  fn parse_group(&mut self, expr: ast::Expr) -> diagnostic::Maybe<ast::Group> {
    // REVISE: No longer independent, because the left parentheses is parsed when disambiguation occurs. Should be independent.

    self.skip_one(&lexer::TokenKind::ParenthesesR)?;

    Ok(ast::Group(expr))
  }

  /// import %name ('::' '{' (%name ',')+ '}')?
  fn parse_import(&mut self) -> diagnostic::Maybe<ast::Import> {
    self.skip_one(&lexer::TokenKind::Import)?;

    let package_name = if self.peek_is(&lexer::TokenKind::ColonDouble) {
      let package_name = self.parse_name()?;

      // Skip past the colon token.
      self.skip()?;

      Some(package_name)
    } else {
      None
    };

    let module_name = self.parse_name()?;

    Ok(ast::Import {
      package_name,
      module_name,
    })
  }

  /// '<' (%name ','))* '>'
  fn parse_generics(&mut self) -> diagnostic::Maybe<ast::Generics> {
    // TODO: This allows for an empty generics list. Add a warning on the semantic check pass, as it is not severe enough to warrant a parser error.

    self.skip_one(&lexer::TokenKind::LessThan)?;

    let mut parameters = Vec::new();

    const TERMINATOR: lexer::TokenKind = lexer::TokenKind::GreaterThan;

    while self.until_terminator(&TERMINATOR)? {
      parameters.push(types::GenericType {
        name: self.parse_name()?,
        registry_id: self.id_generator.next_registry_id(),
        substitution_id: self.id_generator.next_substitution_id(),
      });

      self.skip_comma(&TERMINATOR)?;
    }

    Ok(ast::Generics { parameters })
  }

  /// '<' (%type ',')* '>'
  fn parse_generic_hints(&mut self) -> diagnostic::Maybe<Vec<types::Type>> {
    self.skip_one(&lexer::TokenKind::LessThan)?;

    let mut generic_hints = Vec::new();

    const TERMINATOR: lexer::TokenKind = lexer::TokenKind::GreaterThan;

    // FIXME: This allows for empty generic arguments vector! Should that be allowed?
    while self.until_terminator(&TERMINATOR)? {
      generic_hints.push(self.parse_type()?);
      self.skip_comma(&TERMINATOR)?;
    }

    Ok(generic_hints)
  }

  /// match %expr ':' %indent (%expr '=>' %expr)* '_' '=>' %expr %dedent
  fn parse_match(&mut self) -> diagnostic::Maybe<ast::Match> {
    self.skip_one(&lexer::TokenKind::Match)?;

    let subject = self.parse_expr()?;

    self.skip_many(&[lexer::TokenKind::Colon, lexer::TokenKind::Indent])?;

    let mut cases = Vec::new();
    let default_case;

    loop {
      let is_default_case = self.is(&lexer::TokenKind::Wildcard);

      let expr = if !is_default_case {
        self.parse_expr()?
      } else {
        self.skip()?;
        self.skip_one(&lexer::TokenKind::FatArrow)?;
        default_case = self.parse_expr()?;
        self.skip_one(&lexer::TokenKind::Dedent)?;

        break;
      };

      self.skip_one(&lexer::TokenKind::FatArrow)?;

      let body = self.parse_expr()?;

      cases.push(ast::MatchArm { case: expr, body });
    }

    Ok(ast::Match {
      subject,
      arms: cases,
      default_case,
      subject_type_id: self.id_generator.next_type_id(),
      type_id: self.id_generator.next_type_id(),
    })
  }

  /// '(' (%expr ',')* ')'
  fn parse_tuple(&mut self, first_element_opt: Option<ast::Expr>) -> diagnostic::Maybe<ast::Tuple> {
    // REVISE: No longer independent because of disambiguation phase. Should be independent.

    // Skip the comma left during disambiguation.
    // This comma will not be present when parsing enum variants.
    if self.is(&lexer::TokenKind::Comma) {
      self.skip_one(&lexer::TokenKind::Comma)?;
    }

    let first_element = match first_element_opt {
      Some(first_element) => first_element,
      None => {
        return Ok(ast::Tuple {
          elements: Vec::new(),
          type_id: self.id_generator.next_type_id(),
        })
      }
    };

    let mut elements = vec![first_element];

    while self.until_terminator(&lexer::TokenKind::ParenthesesR)? {
      elements.push(self.parse_expr()?);

      // At least two elements are always required. Otherwise there
      // would be ambiguity with groups. Rust's approach to single-element
      // tuples is simple: `(1,)`.
      self.skip_comma(&lexer::TokenKind::ParenthesesR)?;
    }

    Self::check_llvm_size(elements.len())?;

    Ok(ast::Tuple {
      elements,
      type_id: self.id_generator.next_type_id(),
    })
  }

  /// '(' (%type ',')+ ')'
  fn parse_tuple_type(&mut self) -> diagnostic::Maybe<types::Type> {
    self.skip_one(&lexer::TokenKind::ParenthesesL)?;

    let mut element_types = Vec::new();
    const TERMINATOR: lexer::TokenKind = lexer::TokenKind::ParenthesesR;

    while self.until_terminator(&TERMINATOR)? {
      let element_type = self.parse_type()?;

      element_types.push(element_type);

      // FIXME: A lonely comma should be allowed in this case (to specify a one-element tuple type).
      self.skip_comma(&TERMINATOR)?;
    }

    Ok(types::Type::Tuple(types::TupleType(element_types)))
  }

  /// effect %name (%signature)?
  fn parse_effect(&mut self) -> diagnostic::Maybe<ast::Effect> {
    self.skip_one(&lexer::TokenKind::Effect)?;

    let name = self.parse_name()?;

    let signature = if self.is(&lexer::TokenKind::ParenthesesL) {
      self.parse_signature(SignatureParserInvoker::Effect)?
    } else {
      ast::Signature {
        effects_used: Vec::default(),
        is_variadic: false,
        kind: ast::SignatureKind::Effect,
        parameters: Vec::default(),
        // NOTE: The return type of effects is always `unit`. It shouldn't
        // be left as `None` because it would create a type variable for it
        // during inference, which may cause unexpected behavior (such as the
        // return type's type variable getting unified against a concrete type).
        return_type_hint: Some(types::Type::Unit),
        return_type_id: self.id_generator.next_type_id(),
      }
    };

    Ok(ast::Effect { name, signature })
  }

  /// try %expr (with %effect_handler)+ (default: %effect_handler)?
  fn parse_try(&mut self) -> diagnostic::Maybe<ast::Try> {
    self.skip_one(&lexer::TokenKind::Try)?;

    let expr = self.parse_expr()?;
    let mut handlers = Vec::new();

    // TODO: Require at least one iteration.
    while self.is(&lexer::TokenKind::With) {
      let handler = self.parse_effect_handler(false)?;

      handlers.push(handler);
    }

    // FIXME: This is a temporary measure.
    if handlers.is_empty() {
      return Err(self.expected("at least one effect handler"));
    }

    let default = if self.is(&lexer::TokenKind::Default) {
      self.skip()?;
      self.skip_one(&lexer::TokenKind::Colon)?;

      Some(self.parse_effect_handler(true)?)
    } else {
      None
    };

    Ok(ast::Try {
      expr,
      handlers,
      default,
    })
  }

  fn parse_effect_handler(&mut self, is_default: bool) -> diagnostic::Maybe<ast::EffectHandler> {
    if is_default {
      self.skip_one(&lexer::TokenKind::Default)?;
    } else {
      self.skip_one(&lexer::TokenKind::With)?;
    }

    let name = self.parse_name()?;
    let closure = self.parse_closure()?;

    Ok(ast::EffectHandler { name, closure })
  }

  /// raise %name %arguments
  fn parse_raise(&mut self) -> diagnostic::Maybe<ast::Raise> {
    self.skip_one(&lexer::TokenKind::Raise)?;

    let effect_name = self.parse_name()?;

    // CONSIDER: Creating a `parse_argument` common utility function.
    // TODO: Allow short form as well (name only, without parentheses).

    Ok(ast::Raise {
      effect_name,
      // TODO: Awaiting parsing of arguments.
      arguments: Vec::new(),
    })
  }

  /// resume if %expr
  fn parse_resume(&mut self) -> diagnostic::Maybe<ast::Resume> {
    self.skip_many(&[lexer::TokenKind::Resume, lexer::TokenKind::If])?;

    let condition = self.parse_expr()?;

    Ok(ast::Resume { condition })
  }

  /// '[' %expr ']'
  fn parse_pointer_indexing(
    &mut self,
    pointer: ast::Expr,
  ) -> diagnostic::Maybe<ast::PointerIndexing> {
    self.skip_one(&lexer::TokenKind::BracketL)?;

    let index = self.parse_expr()?;

    self.skip_one(&lexer::TokenKind::BracketR)?;

    Ok(ast::PointerIndexing {
      index,
      pointer,
      type_id: self.id_generator.next_type_id(),
    })
  }

  /// write %expr ',' %expr
  fn parse_pointer_assignment(&mut self) -> diagnostic::Maybe<ast::PointerAssignment> {
    self.skip_one(&lexer::TokenKind::Write)?;

    let pointer = self.parse_expr()?;

    self.skip_one(&lexer::TokenKind::Comma)?;

    let value = self.parse_expr()?;

    Ok(ast::PointerAssignment { pointer, value })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  const TEST_TOKEN_1: lexer::Token = lexer::Token(lexer::TokenKind::Ampersand, 0);
  const TEST_TOKEN_2: lexer::Token = lexer::Token(lexer::TokenKind::And, 1);

  fn create_parser(tokens: &[lexer::TokenKind]) -> Parser {
    // CONSIDER: Making the position incremental.
    Parser::new(
      tokens
        .into_iter()
        .map(|token| lexer::Token(token.to_owned(), 0))
        .collect(),
    )
  }

  #[test]
  fn proper_initial_values() {
    let parser = create_parser(&[]);

    assert_eq!(0, parser.index);
    assert!(parser.tokens.is_empty());
  }

  #[test]
  fn is_index_out_of_bounds() {
    let mut parser = create_parser(&[]);

    assert!(parser.is_index_out_of_bounds());
    parser.tokens.push(TEST_TOKEN_1);
    assert!(!parser.is_index_out_of_bounds());
    parser.tokens.push(TEST_TOKEN_2);
    assert!(!parser.is_index_out_of_bounds());
    parser.index += 1;
    assert!(!parser.is_index_out_of_bounds());
    parser.index += 1;
    assert!(parser.is_index_out_of_bounds());
    parser.index += 1;
    assert!(parser.is_index_out_of_bounds());
  }

  #[test]
  fn is() {
    let mut parser = create_parser(&[]);

    assert!(parser.is_at_last_token_or_past());
    parser.index = 1;
    assert!(parser.is_at_last_token_or_past());
    parser.index = 0;
    parser.tokens.push(lexer::Token(lexer::TokenKind::Func, 0));
    assert!(parser.is(&lexer::TokenKind::Func));
  }

  #[test]
  fn skip() {
    let mut parser = create_parser(&[lexer::TokenKind::Func, lexer::TokenKind::Func]);

    assert!(parser.skip().is_ok());
    assert_eq!(1, parser.index);
    assert!(parser.skip().is_ok());
    assert!(parser.skip().is_err());
  }

  #[test]
  fn skip_one() {
    const TOKEN_KIND: lexer::TokenKind = lexer::TokenKind::Func;
    let mut parser = create_parser(&[TOKEN_KIND]);

    assert!(parser.skip_one(&lexer::TokenKind::Ampersand).is_err());
    assert!(parser.skip_one(&TOKEN_KIND).is_ok());
  }

  #[test]
  fn skip_many() {
    const TOKEN_KIND: lexer::TokenKind = lexer::TokenKind::Func;

    let mut parser = create_parser(&[
      TOKEN_KIND,
      TOKEN_KIND,
      TOKEN_KIND,
      lexer::TokenKind::Ampersand,
      lexer::TokenKind::And,
    ]);

    assert!(parser.skip_many(&[TOKEN_KIND]).is_ok());
    assert!(parser.skip_many(&[TOKEN_KIND, TOKEN_KIND]).is_ok());
    assert!(parser.skip_many(&[TOKEN_KIND]).is_err());

    assert!(parser
      .skip_many(&[lexer::TokenKind::Ampersand, lexer::TokenKind::And])
      .is_ok());
  }

  #[test]
  fn skip_out_of_bounds() {
    let mut parser = create_parser(&[lexer::TokenKind::Func]);

    assert!(parser.skip().is_ok());
    assert_eq!(1, parser.index);
    assert!(parser.skip().is_err());
    assert_eq!(1, parser.index);
  }

  #[test]
  fn is_pre_eof() {
    let mut parser = create_parser(&[]);

    assert!(parser.is_at_last_token_or_past());
    parser.tokens.push(lexer::Token(lexer::TokenKind::Func, 0));
    assert!(parser.is_at_last_token_or_past());
    parser.tokens.push(lexer::Token(lexer::TokenKind::Func, 0));
    assert!(!parser.is_at_last_token_or_past());
    assert!(parser.skip().is_ok());
    assert!(parser.is_at_last_token_or_past());
  }

  #[test]
  fn is_binary_operator() {
    assert!(Parser::is_binary_operator_token(&lexer::TokenKind::Plus));

    assert!(Parser::is_binary_operator_token(
      &lexer::TokenKind::Equality
    ));

    assert!(Parser::is_binary_operator_token(&lexer::TokenKind::And));
    assert!(!Parser::is_binary_operator_token(&lexer::TokenKind::BraceL));
    assert!(!Parser::is_binary_operator_token(&lexer::TokenKind::Not));
  }

  #[test]
  fn peek() {
    let mut parser = create_parser(&[]);

    assert!(parser.peek().is_none());
    parser.tokens.push(TEST_TOKEN_1);
    parser.tokens.push(TEST_TOKEN_2);
    assert_eq!(parser.peek(), Some(&TEST_TOKEN_2.0));
  }

  #[test]
  fn peek_is() {
    let mut parser = create_parser(&[]);

    assert!(!parser.peek_is(&lexer::TokenKind::BraceL));

    parser
      .tokens
      .push(lexer::Token(lexer::TokenKind::BraceL, 0));

    assert!(!parser.peek_is(&lexer::TokenKind::BraceL));

    parser
      .tokens
      .push(lexer::Token(lexer::TokenKind::BraceL, 0));

    assert!(parser.peek_is(&lexer::TokenKind::BraceL));
  }

  #[test]
  fn is_promotion_chain() {
    let mut parser = create_parser(&[]);

    assert!(!parser.is_promotion_chain());
    parser.tokens.push(lexer::Token(lexer::TokenKind::Dot, 0));
    assert!(parser.is_promotion_chain());
    parser.tokens.pop();
    parser.tokens.push(lexer::Token(lexer::TokenKind::Func, 0));
    assert!(!parser.is_promotion_chain());
  }

  #[test]
  fn is_unary_operator() {
    let mut parser = create_parser(&[]);

    assert!(!parser.is_unary_operator_token());
    parser.tokens.push(lexer::Token(lexer::TokenKind::Not, 0));
    assert!(parser.is_unary_operator_token());
    parser.tokens.pop();
    parser.tokens.push(lexer::Token(lexer::TokenKind::Func, 0));
    assert!(!parser.is_unary_operator_token());
  }

  #[test]
  fn expected() {
    let parser = create_parser(&[]);
    const EXPECTED_TOKEN: &str = "test";
    let result = parser.expected(EXPECTED_TOKEN);

    assert_eq!(result.len(), 1);

    let only_diagnostic = if let Some(first) = result.first() {
      first
    } else {
      panic!("Expected at least one diagnostic.");
    };

    assert!(matches!(
      only_diagnostic,
      diagnostic::Diagnostic::ExpectedButGotToken(..)
    ));
  }

  #[test]
  fn get_token() {
    let mut parser = create_parser(&[]);

    assert!(parser.get_token().is_err());
    parser.tokens.push(TEST_TOKEN_1);
    // FIXME: Fix test.
    // assert_eq!(parser.get_token(), Ok(&TEST_TOKEN_1.0));
    parser.tokens.push(TEST_TOKEN_2);
    parser.index += 1;
    // FIXME: Fix test.
    // assert_eq!(parser.get_token(), Ok(&TEST_TOKEN_2.0));
  }

  #[test]
  fn until() {
    let mut parser = create_parser(&[]);

    assert!(parser.until(&TEST_TOKEN_1.0).is_err());
    parser.tokens.push(TEST_TOKEN_2);
    // FIXME: Fix test.
    // assert_eq!(parser.until(&TEST_TOKEN_1.0), Ok(true));
    // assert_eq!(parser.until(&TEST_TOKEN_2.0), Ok(false));
  }

  // FIXME: Fix test.
  // #[test]
  // fn until_terminator() {
  //   let mut parser = create_parser(&[]);
  //   const TERMINATOR: lexer::TokenKind = TEST_TOKEN_2.0;

  //   assert!(parser.until_terminator(&TERMINATOR).is_err());
  //   parser.tokens.push(TEST_TOKEN_1);
  //   assert_eq!(parser.until_terminator(&TERMINATOR), Ok(true));
  //   parser.tokens.pop();
  //   parser.tokens.push(TEST_TOKEN_2);
  //   assert_eq!(parser.until_terminator(&TERMINATOR), Ok(false));
  //   assert!(parser.is_index_out_of_bounds());
  // }

  #[test]
  fn check_llvm_size() {
    const OK_SIZE: usize = 0;
    const BAD_SIZE: usize = usize::MAX;

    assert!(Parser::check_llvm_size(OK_SIZE).is_ok());
    assert!(Parser::check_llvm_size(BAD_SIZE).is_err());
  }

  #[test]
  fn get_llvm_size() {
    assert!(Parser::get_llvm_size(0).is_ok());
    assert!(Parser::get_llvm_size(1).is_ok());
    assert!(Parser::get_llvm_size(2).is_ok());
    assert!(Parser::get_llvm_size(u32::MAX as usize).is_ok());
    assert!(Parser::get_llvm_size(usize::MAX).is_err());
    assert!(Parser::get_llvm_size(u32::MAX as usize + 1).is_err());
  }

  #[test]
  fn minimum_bit_width_of() {
    assert_eq!(
      Parser::minimum_bit_width_of(&0f64),
      Ok(types::BitWidth::Width8)
    );

    assert_eq!(
      Parser::minimum_bit_width_of(&(i16::MAX as f64)),
      Ok(types::BitWidth::Width16)
    );

    assert_eq!(
      Parser::minimum_bit_width_of(&(i32::MAX as f64)),
      Ok(types::BitWidth::Width32)
    );
  }

  // TODO: Add more tests.
}
