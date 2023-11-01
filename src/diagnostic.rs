//! Diagnostics are user-facing errors whose purpose is to inform the user of any kind of
//! problem that might arise (except for logic bugs, or internal errors). In other words,
//! diagnostics can be seen as the compiler's approach to error handling and reporting.

use crate::{pass, symbol_table, types};

/// A function that may produce multiple diagnostics which are visible to the
/// end user, in the case of its failure.
///
/// Any function with this return type can be considered a high-level function,
/// and should also be allowed to make assumptions (ie. if contract assumptions
/// are to be broken, it constitutes a logic bug, which is a valid situation in
/// which panics may occur).
pub type Maybe<T = ()> = Result<T, Vec<Diagnostic>>;

// REVISE: Expand certain variants into objects with field names if they have two or more fields. This is for code readability and clarity.
#[derive(Debug, Clone)]
pub enum Diagnostic {
  FunctionMissingGenericHints(String),
  ReturnTypeHintRequired,
  ClosureCaptureAfterParameters,
  ParameterTypeHintRequired(String),
  NonAsciiCharactersNotSupported(char),
  CalleeCannotAcceptGenericHints(String),
  RecursiveType(types::Type),
  IntersectionOfClosedObjectsIsIncomplete(usize, usize),
  GenericParameterCountMismatch {
    expected: usize,
    actual: usize,
  },
  OpaquePointerMustBeCasted,
  ConstructionOfInfiniteType,
  SignaturesDifferInParameterCount(usize, usize),
  ObjectTypeMismatch,
  TypeMismatch(types::Type, types::Type),
  TargetFieldDoesNotExist(String),
  /// A type variable could not be solved, and it suggests that type annotations
  /// might be needed.
  UnsolvedTypeVariable(symbol_table::SubstitutionId, String),
  FunctionsCannotBeVariadic(String),
  ExpectedButGotCharacter(char, char),
  MainFunctionSignatureMismatch,
  RangeStartMustBeLessOrEqualToEnd(u64, u64),
  ExpectedButGotToken(String, String),
  UnexpectedlyReachedEndOfFile,
  NumberLiteralTooBig,
  InvalidEscapeSequence(char),
  CannotUseOutsideUnsafe,
  Redefinition(String),
  Redeclaration(symbol_table::SymbolPath),
  SymbolCannotBeShadowed(String),
  QualifiedSymbolNotFound(String),
  MultipleEntryPoints,
  UndefinedReference(String),
  InvalidCastType,
  RedundantCast,
  UnexpectedEndOfInputExpectedChar,
  ObjectsDifferInFieldCount,
  ObjectsDifferInFieldName,
  FunctionBodyMustYield(String),
  InvalidIndexingTarget,
  CannotYieldTemporaryReference,
  BindingUsedAfterMove(String),
  TuplesDifferInLength,
  UnionTypesDiffer,
  NestedUnsafeScopes,
  ConditionOrValueIsConstant,
  BlocksMustHaveAtLeastOneStatement,
  MissingEntryPoint,
  LifetimeViolation {
    which_binding: symbol_table::RegistryId,
    at: symbol_table::RegistryId,
    starts_at: symbol_table::RegistryId,
  },
  FunctionTakesNoGenericParameters(String),
  UnusedValueMustBeUsedOrDiscarded,
  TupleAccessOutOfBounds {
    index: usize,
    tuple_length: usize,
  },
  ObjectFieldCountMismatch(usize, usize),
  ObjectFieldDoesNotExist(String),
  ConstantValueNotConstant,
  CountOrSizeTooLarge,
  RepeatedObjectField(String),
}

impl Diagnostic {
  pub fn is_warning(&self) -> bool {
    matches!(
      self,
      Diagnostic::RedundantCast
        | Diagnostic::NestedUnsafeScopes
        | Diagnostic::ConditionOrValueIsConstant
    )
  }

  pub fn is_error(&self) -> bool {
    !self.is_warning()
  }
}

#[derive(Default, Clone)]
pub struct DiagnosticsHelper {
  pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticsHelper {
  pub fn try_add_one(&mut self, diagnostic: Diagnostic) -> Maybe {
    let is_error = diagnostic.is_error();

    self.diagnostics.push(diagnostic);

    if is_error {
      Err(self.diagnostics.clone())
    } else {
      Ok(())
    }
  }

  pub fn check(&self) -> Maybe {
    if self.contains_errors() {
      Err(self.diagnostics.clone())
    } else {
      Ok(())
    }
  }

  pub fn extend(&mut self, result: Maybe) -> Maybe {
    if let Err(diagnostics) = result {
      self.diagnostics.extend(diagnostics);
    }

    self.check()
  }

  pub fn add_one(&mut self, diagnostic: Diagnostic) {
    self.diagnostics.push(diagnostic);
  }

  pub fn add_many(&mut self, diagnostics: Vec<Diagnostic>) {
    self.diagnostics.extend(diagnostics);
  }

  pub fn contains_errors(&self) -> bool {
    self.diagnostics.iter().any(Diagnostic::is_error)
  }

  pub fn try_return_value<T>(self, value: T) -> Maybe<T> {
    if self.contains_errors() {
      Err(self.diagnostics)
    } else {
      Ok(value)
    }
  }

  pub fn into_pass_result(self) -> pass::PassResult {
    if self.contains_errors() {
      pass::PassResult::Err(self.diagnostics)
    } else {
      pass::PassResult::Ok(self.diagnostics)
    }
  }
}

impl std::convert::From<Vec<Diagnostic>> for DiagnosticsHelper {
  fn from(diagnostics: Vec<Diagnostic>) -> Self {
    Self { diagnostics }
  }
}
