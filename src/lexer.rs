//! The lexer is responsible for accepting an input string of source code and
//! converting it into a stream of tokens. It is also responsible for handling
//! indentation, which is used to determine the scope of blocks of code.
//!
//! Once the lexing process has finished, the parser will take the stream of
//! tokens produced and transform it into an abstract syntax tree (AST).

use crate::diagnostic;

/// Represents a single unit of a program. The structure contains
/// the token's kind, followed by its absolute start position. The
/// end position can be computed by using the token kind's value.
pub struct Token(pub TokenKind, pub usize);

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
  Indent,
  Dedent,
  Illegal(char),
  Identifier(String),
  Whitespace(char),
  Comment(String),
  String(String),
  Number(f64, bool),
  Bool(bool),
  Char(char),
  Null,
  Func,
  Foreign,
  Let,
  If,
  Else,
  Unsafe,
  Enum,
  And,
  Or,
  Nand,
  Nor,
  Xor,
  Type,
  TypeInt8,
  TypeInt16,
  TypeInt32,
  TypeInt64,
  TypeNat8,
  TypeNat16,
  TypeNat32,
  TypeNat64,
  TypeReal16,
  TypeReal32,
  TypeReal64,
  TypeBool,
  TypeString,
  TypeUnit,
  TypeOpaque,
  TypeChar,
  BraceL,
  BraceR,
  ParenthesesL,
  ParenthesesR,
  Colon,
  ColonDouble,
  Ampersand,
  Comma,
  Plus,
  Minus,
  Asterisk,
  Slash,
  Bang,
  Not,
  Equal,
  LessThan,
  GreaterThan,
  BracketL,
  BracketR,
  Dot,
  At,
  Backtick,
  Arrow,
  LessThanEqualTo,
  GreaterThanEqualTo,
  Equality,
  Inequality,
  FatArrow,
  EllipsisLong,
  EllipsisShort,
  Sizeof,
  Pipe,
  Const,
  Elif,
  In,
  This,
  Import,
  As,
  Wildcard,
  Do,
  Match,
  Var,
  Effect,
  Try,
  With,
  Resume,
  SemiColon,
  Discard,
  Assignment,
  StarAssign,
  VerticalBar,
  Pass,
  PercentSign,
  Default,
  Uses,
  Raise,
}

pub struct Lexer {
  input: Vec<char>,
  index: usize,
  /// Represents the current character.
  ///
  /// If the input string was empty, or if the index is out of
  /// bounds, it will be `None`.
  current_char: Option<char>,
  seen_only_whitespace_this_line: bool,
  indent_level: usize,
  indent_counter: usize,
}

// TODO: Handle unicode vs. ASCII. Consider emitting error string (`Result`) when a unicode (or non-ASCII) character is encountered.
impl Lexer {
  const COMMENT_CHAR: char = '-';
  const NEWLINE_CHAR: char = '\n';

  fn match_identifier(identifier: &str) -> Option<TokenKind> {
    Some(match identifier {
      "func" => TokenKind::Func,
      "foreign" => TokenKind::Foreign,
      "let" => TokenKind::Let,
      "if" => TokenKind::If,
      "else" => TokenKind::Else,
      "unsafe" => TokenKind::Unsafe,
      "enum" => TokenKind::Enum,
      "and" => TokenKind::And,
      "or" => TokenKind::Or,
      "nand" => TokenKind::Nand,
      "nor" => TokenKind::Nor,
      "xor" => TokenKind::Xor,
      "type" => TokenKind::Type,
      "null" => TokenKind::Null,
      "int8" => TokenKind::TypeInt8,
      "int16" => TokenKind::TypeInt16,
      "int" => TokenKind::TypeInt32,
      "int64" => TokenKind::TypeInt64,
      "nat8" => TokenKind::TypeNat8,
      "nat16" => TokenKind::TypeNat16,
      "nat" => TokenKind::TypeNat32,
      "nat64" => TokenKind::TypeNat64,
      "real16" => TokenKind::TypeReal16,
      "real" => TokenKind::TypeReal32,
      "real64" => TokenKind::TypeReal64,
      "bool" => TokenKind::TypeBool,
      "str" => TokenKind::TypeString,
      "this" => TokenKind::This,
      "unit" => TokenKind::TypeUnit,
      "opaque" => TokenKind::TypeOpaque,
      "char" => TokenKind::TypeChar,
      "true" => TokenKind::Bool(true),
      "false" => TokenKind::Bool(false),
      "import" => TokenKind::Import,
      "sizeof" => TokenKind::Sizeof,
      "const" => TokenKind::Const,
      "elif" => TokenKind::Elif,
      "in" => TokenKind::In,
      "as" => TokenKind::As,
      "_" => TokenKind::Wildcard,
      "do" => TokenKind::Do,
      "match" => TokenKind::Match,
      "var" => TokenKind::Var,
      "effect" => TokenKind::Effect,
      "try" => TokenKind::Try,
      "with" => TokenKind::With,
      "resume" => TokenKind::Resume,
      "discard" => TokenKind::Discard,
      "not" => TokenKind::Not,
      "pass" => TokenKind::Pass,
      "default" => TokenKind::Default,
      "uses" => TokenKind::Uses,
      "raise" => TokenKind::Raise,
      _ => return None,
    })
  }

  /// Determine whether a character is a letter, and within the range
  /// of `a-Z`, or is `_`.
  fn is_letter(character: char) -> bool {
    ('a' <= character && character <= 'z')
      || ('A' <= character && character <= 'Z')
      || character == '_'
  }

  /// Determine if a character is a digit within the range of 0-9.
  fn is_digit(character: char) -> bool {
    character.is_digit(10)
  }

  /// Determine if a character is a whitespace character.
  fn is_whitespace(character: char) -> bool {
    matches!(character, ' ' | '\t' | '\n' | '\r')
  }

  pub fn lex_all(input: &str) -> diagnostic::Maybe<Vec<Token>> {
    let mut lexer = Lexer::new(input.chars().collect());
    let mut tokens = Vec::new();

    while let Some(token) = lexer.lex_token()? {
      tokens.push(token);
    }

    Ok(tokens)
  }

  pub fn new(input: Vec<char>) -> Self {
    // OPTIMIZE: `Vec<char>` might be inefficient since it needs to be cloned. Consider using `&str` instead, or some other reference type.

    let current_char = if input.is_empty() {
      None
    } else {
      Some(input[0])
    };

    Self {
      input,
      index: 0,
      current_char,
      seen_only_whitespace_this_line: true,
      indent_level: 0,
      indent_counter: 0,
    }
  }

  /// Set the current character buffer to the character on the next
  /// index.
  ///
  /// If there are no more characters, the current character buffer
  /// will be set to `None` to indicate the end of the input string.
  fn read_char(&mut self) -> Option<char> {
    if self.index + 1 >= self.input.len() {
      self.current_char = None;

      return None;
    }

    self.index += 1;
    self.current_char = Some(self.input[self.index]);

    self.current_char
  }

  fn read_over(&mut self, expected_char: char) -> diagnostic::Maybe {
    if self.current_char != Some(expected_char) {
      return Err(vec![diagnostic::Diagnostic::ExpectedButGotCharacter(
        expected_char,
        self.current_char.unwrap_or('\0'),
      )]);
    }

    self.read_char();

    Ok(())
  }

  /// Determine if the current character is unset, and therefore
  /// signifies the end of the input string.
  fn is_eof(&self) -> bool {
    self.current_char.is_none()
  }

  /// Determine if there is a next character, by comparing the current
  /// index with the input's length.
  fn is_next_eof(&self) -> bool {
    self.index + 1 >= self.input.len()
  }

  /// Reads characters from the input sequence until the `predicate` function
  /// returns `false` for the current character. Returns the read characters as
  /// a `String`.
  ///
  /// If `EOF` is reached, this function will stop reading characters, and any
  /// previously consumed result will be returned.
  fn read_while(&mut self, predicate: fn(char) -> bool) -> String {
    let mut result = String::new();

    while let Some(character) = self.current_char {
      if !predicate(character) {
        break;
      }

      result.push(character);
      self.read_char();
    }

    result
  }

  /// Reads an identifier from the input sequence.
  ///
  /// An identifier consists of one or more letters, digits, or underscores,
  /// followed optionally by a single quote character (') at the end.
  ///
  /// Returns the resulting identifier as a `String`.
  fn lex_identifier(&mut self) -> String {
    let mut result = self.read_while(|character| -> bool {
      Self::is_letter(character) || Self::is_digit(character) || character == '_'
    });

    // Special case to allow the `'` character as the last character
    // in an identifier. This allows the programmer to overcome name
    // conflicts with common keywords.
    if self.current_char == Some('\'') {
      self.read_char();
      result += "'";
    }

    result
  }

  /// Reads a number from the input sequence.
  ///
  /// A number consists of an integral part, which is one or more digits, followed
  /// optionally by a fractional part, which is a decimal point followed by one or
  /// more digits.
  ///
  /// Returns the read number as a tuple of the form `(f64, bool)`, where the first
  /// element is the parsed number and the second element is a flag indicating whether
  /// the number has a fractional part (i.e., whether it is a real number or an integer).
  ///
  /// # Errors
  ///
  /// If the number is too large or otherwise invalid, a diagnostic will be returned.
  fn lex_number(&mut self) -> diagnostic::Maybe<(f64, bool)> {
    let integral_component = self.read_while(Self::is_digit);

    let fractional_component = if self.current_char == Some('.') {
      self.read_char();

      Some(self.read_while(Self::is_digit))
    } else {
      None
    };

    let (number_result, is_real) = if let Some(fractional_part) = fractional_component {
      ((integral_component + &fractional_part).parse::<f64>(), true)
    } else {
      (integral_component.parse::<f64>(), false)
    };

    if let Ok(number) = number_result {
      Ok((number, is_real))
    } else {
      Err(vec![diagnostic::Diagnostic::NumberLiteralTooBig])
    }
  }

  /// Reads a comment from the input sequence.
  ///
  /// A comment is a sequence of characters that starts with the `--`
  /// symbols and continues until the next newline character (`\n`).
  ///
  /// Returns the read comment as a `String`, excluding the first two
  /// characters of `--`.
  fn lex_comment(&mut self) -> diagnostic::Maybe<String> {
    self.read_over(Self::COMMENT_CHAR)?;
    self.read_over(Self::COMMENT_CHAR)?;

    Ok(self.read_while(|character| character != '\n'))
  }

  /// Reads a string literal from the input sequence. A string literal is a sequence
  /// of characters enclosed in double-quote characters (`"`) and may contain
  /// escape sequences for special characters (e.g., `\n` for a newline). Returns
  /// the parsed string as a `String`.
  ///
  /// # Errors
  ///
  /// If the input sequence contains an invalid escape sequence or the string
  /// literal is not properly terminated with a closing double-quote character,
  /// a diagnostic will be returned.
  fn lex_string(&mut self) -> diagnostic::Maybe<String> {
    // TODO: Implement unit tests for this.

    // Skip the opening double-quote.
    self.read_over('"')?;

    let mut string = String::new();

    loop {
      string.push_str(&self.read_while(|char| char != '"' && char != '\\'));

      // The end of the string has been reached.
      if self.current_char == Some('"') {
        break;
      }

      // Otherwise, there is an escape sequence. Skip the escape character.
      self.read_char();

      // TODO: Escape sequences should also be possible for chars (single-characters). Will need to abstract this functionality into its own function at that point.
      string += match self.current_char {
        Some('n') => "\n",
        Some('t') => "\t",
        Some('r') => "\r",
        Some('\\') => "\\",
        Some('0') => "\0",
        // TODO: Add unicode escape codes ('\u{xxxx}' or '\u{xx}'). This allows for emojis and unicode.
        Some(char) => return Err(vec![diagnostic::Diagnostic::InvalidEscapeSequence(char)]),
        None => {
          return Err(vec![
            diagnostic::Diagnostic::UnexpectedEndOfInputExpectedChar,
          ]);
        }
      };

      // Move on from the matched character (if any).
      self.read_char();
    }

    // Skip the closing double-quote.
    self.read_over('"')?;

    Ok(string)
  }

  /// Reads a character literal from the input sequence. A character literal is a
  /// single character enclosed in single-quote characters (`'`). Returns the
  /// parsed character as a `char`.
  fn lex_character(&mut self) -> diagnostic::Maybe<char> {
    // Skip the opening single-quote, and attempt to retrieve the character.
    self.read_over('\'')?;

    let value = self.current_char.ok_or(vec![
      diagnostic::Diagnostic::UnexpectedEndOfInputExpectedChar,
    ])?;

    // Skip the character.
    self.read_char();

    // Skip the closing single-quote.
    self.read_over('\'')?;

    Ok(value)
  }

  /// Returns the next character in the input sequence without advancing the
  /// current index. If the current position is located at the end of the input
  /// sequence, `None` is returned instead.
  fn peek_char(&self) -> Option<char> {
    if self.is_next_eof() {
      return None;
    }

    Some(self.input[self.index + 1])
  }

  /// Determine whether the current character in the input sequence is the first
  /// character of an indent. An indent is a sequence of two or more spaces
  /// at the beginning of a line.
  fn is_indent(&self) -> bool {
    self.seen_only_whitespace_this_line
      && (self.current_char == Some(' ') && self.peek_char() == Some(' '))
  }

  /// Attempts to lex an indent or dedent from the input sequence. If the current
  /// character is the start of an indent, the indent is read and returned as a
  /// `TokenKind`. If the current character is the start of a dedent, the dedent
  /// is read and returned as a `TokenKind`. Otherwise, `None` is returned.
  fn try_lex_indentation(&mut self) -> Option<TokenKind> {
    if self.indent_counter > self.indent_level {
      self.indent_level = self.indent_counter;

      return Some(TokenKind::Indent);
    } else if self.indent_counter < self.indent_level {
      self.indent_level -= 1;

      return Some(TokenKind::Dedent);
    }

    None
  }

  /// Attempts to lex a comment from the input sequence. If the current character
  /// is the start of a comment, the comment is read and returned.
  /// Otherwise, `None` is returned.
  fn try_lex_comment(&mut self) -> diagnostic::Maybe<Option<TokenKind>> {
    if self.current_char != Some(Self::COMMENT_CHAR) || self.peek_char() != Some(Self::COMMENT_CHAR)
    {
      return Ok(None);
    }

    let comment_token = TokenKind::Comment(self.lex_comment()?);

    // Reset the indentation counter immediately after reading a comment.
    self.indent_counter = 0;

    return Ok(Some(comment_token));
  }

  /// Upon beginning to lex a new line, the indentation counter is reset to 0.
  /// Invoking this method will update the indentation counter to the current
  /// indentation level at the current line.
  fn update_indentation_counter(&mut self) {
    // While the current character is considered an indent (two spaces), skip
    // the two characters and increment the indent counter. This is done to
    // count up to the current indent level at the current line.
    while self.is_indent() {
      // Two spaces represent an indent.
      self.read_char();
      self.read_char();

      self.indent_counter += 1;
    }
  }

  fn lex_next_token_kind(&mut self, char: char) -> diagnostic::Maybe<TokenKind> {
    let token_kind = match char {
      // REVIEW: Why is there an early return for string, but not for the other cases? Document why this is the case by adding a note.
      '"' => return Ok(TokenKind::String(self.lex_string()?)),
      '\'' => return Ok(TokenKind::Char(self.lex_character()?)),
      '%' => TokenKind::PercentSign,
      ';' => TokenKind::SemiColon,
      '{' => TokenKind::BraceL,
      '}' => TokenKind::BraceR,
      '(' => TokenKind::ParenthesesL,
      ')' => TokenKind::ParenthesesR,
      '|' if self.peek_char() == Some('>') => {
        self.read_char();

        TokenKind::Pipe
      }
      '|' => TokenKind::VerticalBar,
      ':' if self.peek_char() == Some(':') => {
        self.read_char();

        TokenKind::ColonDouble
      }
      ':' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::Assignment
      }
      ':' => TokenKind::Colon,
      '&' => TokenKind::Ampersand,
      ',' => TokenKind::Comma,
      '+' => TokenKind::Plus,
      '-' if self.peek_char() == Some('>') => {
        self.read_char();

        TokenKind::Arrow
      }
      '-' => TokenKind::Minus,
      '*' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::StarAssign
      }
      '*' => TokenKind::Asterisk,
      '/' => TokenKind::Slash,
      '!' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::Inequality
      }
      '!' => TokenKind::Bang,
      '=' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::Equality
      }
      '=' if self.peek_char() == Some('>') => {
        self.read_char();

        TokenKind::FatArrow
      }
      '=' => TokenKind::Equal,
      '<' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::LessThanEqualTo
      }
      '<' => TokenKind::LessThan,
      '>' if self.peek_char() == Some('=') => {
        self.read_char();

        TokenKind::LessThanEqualTo
      }
      '>' => TokenKind::GreaterThan,
      '[' => TokenKind::BracketL,
      ']' => TokenKind::BracketR,
      '.' if self.peek_char() == Some('.') && self.peek_char() == Some('.') => {
        self.read_char();
        self.read_char();

        TokenKind::EllipsisLong
      }
      '.' if self.peek_char() == Some('.') => {
        self.read_char();

        TokenKind::EllipsisShort
      }
      '.' => TokenKind::Dot,
      '@' => TokenKind::At,
      '`' => TokenKind::Backtick,
      '_' if self.peek_char().map(Self::is_letter).unwrap_or(false) => TokenKind::Wildcard,
      _ => {
        // If none of the symbols matched, first attempt to read an identifier.
        // Note that identifiers will never start with a digit.
        return if char == '_' || Self::is_letter(char) {
          let identifier = self.lex_identifier();

          match Self::match_identifier(&identifier) {
            Some(keyword_token) => Ok(keyword_token),
            None => Ok(TokenKind::Identifier(identifier)),
          }
        }
        // Otherwise, attempt to read a number.
        else if Self::is_digit(char) {
          let number_result = self.lex_number()?;

          Ok(TokenKind::Number(number_result.0, number_result.1))
        }
        // Finally, at this point the character is considered illegal.
        else {
          let illegal_char = char;

          self.read_char();

          Ok(TokenKind::Illegal(illegal_char))
        };
      }
    };

    self.read_char();

    Ok(token_kind)
  }

  /// Attempt to consume and return the next token.
  ///
  /// If the end of the input string has been reached, `None` will be
  /// returned. If the current character is neither an identifier nor a
  /// digit, an [`Illegal`] token with the encountered character as its
  /// value will be returned instead.
  fn lex_token_kind(&mut self) -> diagnostic::Maybe<Option<TokenKind>> {
    // If `EOF` has been reached, and the indent level is above zero, the
    // last token to be produced is a dedent. Otherwise, simply return the
    // `EOF` token.
    if self.is_eof() {
      return Ok(if self.indent_level > 0 {
        self.indent_level -= 1;

        Some(TokenKind::Dedent)
      } else {
        None
      });
    }

    // TODO: Cleanup logic. This buffer might cause logic bugs (has already caused one!).
    // REVISE: Don't use a buffer since it might cause logic bugs after the actual current character changes. Be careful.
    let mut current_char = self
      .current_char
      .expect("buffer should be set because of the `is_eof` guard");

    // Prevent non-ASCII characters from being lexed, such as unicode
    // or emojis. In the future, this may be allowed. Note that since
    // this does not prevent unicode from being used inside strings
    // (as intended), but only as part of identifiers, keywords, etc.
    if !current_char.is_ascii() {
      return Err(vec![
        diagnostic::Diagnostic::NonAsciiCharactersNotSupported(current_char),
      ]);
    }

    // TODO: Explain what this is doing. It is not immediately clear.
    if !Self::is_whitespace(current_char) && self.seen_only_whitespace_this_line {
      self.seen_only_whitespace_this_line = false;
    }
    // Reset the indent counter upon reaching the end of each line.
    else if current_char == Self::NEWLINE_CHAR {
      self.seen_only_whitespace_this_line = true;
      self.indent_counter = 0;
      self.read_char();

      return Ok(Some(TokenKind::Whitespace(Self::NEWLINE_CHAR)));
    }

    self.update_indentation_counter();

    // Update the current character, since it has changed because of the
    // `read_char` sub-calls. Otherwise if there are no more characters, then
    // `EOF` has been reached.
    if let Some(updated_current_char) = self.current_char {
      current_char = updated_current_char;
    } else {
      return Ok(None);
    }

    // Handle comments before indentation to prevent them from triggering
    // indentation level changes.
    if let Some(comment_token) = self.try_lex_comment()? {
      return Ok(Some(comment_token));
    }

    if let Some(indent_token) = self.try_lex_indentation() {
      return Ok(Some(indent_token));
    }

    // If the current character is whitespace, skip it and return a
    // whitespace token.
    if Self::is_whitespace(current_char) {
      self.read_char();

      return Ok(Some(TokenKind::Whitespace(current_char)));
    }

    // Otherwise, proceed to lex the next token kind.
    let next_token_kind = self.lex_next_token_kind(current_char)?;

    Ok(Some(next_token_kind))
  }

  fn lex_token(&mut self) -> diagnostic::Maybe<Option<Token>> {
    let start = self.index;
    let token_kind = self.lex_token_kind()?;

    Ok(token_kind.map(|token_kind| Token(token_kind, start)))
  }
}

impl From<&str> for Lexer {
  fn from(string: &str) -> Self {
    Self::new(string.chars().collect())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  fn assert_sequence(lexer: &mut Lexer, expected_tokens: &[TokenKind]) {
    for expected_token in expected_tokens {
      match lexer.lex_token_kind() {
        Err(diagnostic) => {
          panic!(
            "token sequence assertion failed with diagnostic: {:?}",
            diagnostic
          );
        }
        Ok(actual_token) => assert_eq!(actual_token.as_ref(), Some(expected_token)),
      };
    }
  }

  #[test]
  fn is_letter() {
    assert_eq!(true, Lexer::is_letter('a'));
    assert_eq!(true, Lexer::is_letter('z'));
    assert_eq!(true, Lexer::is_letter('_'));
    assert_eq!(false, Lexer::is_letter('0'));
    assert_eq!(false, Lexer::is_letter('1'));
    assert_eq!(false, Lexer::is_letter('!'));
  }

  #[test]
  fn is_digit() {
    assert_eq!(false, Lexer::is_digit('a'));
    assert_eq!(false, Lexer::is_digit('z'));
    assert_eq!(false, Lexer::is_digit('_'));
    assert_eq!(true, Lexer::is_digit('0'));
    assert_eq!(true, Lexer::is_digit('1'));
    assert_eq!(false, Lexer::is_digit('!'));
  }

  #[test]
  fn proper_initial_values() {
    let lexer = Lexer::new(vec!['a']);

    assert_eq!(lexer.input.len(), 1);
    assert_eq!(lexer.input[0], 'a');
    assert_eq!(lexer.index, 0);
    assert_eq!(lexer.current_char, Some('a'));
  }

  #[test]
  fn lex_identifier_single_char() {
    let mut lexer = Lexer::new(vec!['a']);

    assert!(matches!(
      lexer.lex_token_kind(),
      Ok(Some(TokenKind::Identifier(..)))
    ));
  }

  #[test]
  fn lex_identifier_after_before() {
    let mut lexer = Lexer::new(vec![' ', 'a', 'b', 'c', ' ']);

    assert_eq!(true, lexer.lex_token_kind().is_ok());

    assert!(matches!(
      lexer.lex_token_kind(),
      Ok(Some(TokenKind::Identifier(..)))
    ));
  }

  #[test]
  fn lex_identifier() {
    let mut lexer = Lexer::new(vec!['a', 'b', 'c']);

    assert!(matches!(
      lexer.lex_token_kind(),
      Ok(Some(TokenKind::Identifier(..)))
    ));
  }

  #[test]
  fn lex_identifier_prime() {
    let mut lexer = Lexer::new(vec!['a', 'b', 'c', '\'']);

    assert!(matches!(
      lexer.lex_token_kind(),
      Ok(Some(TokenKind::Identifier(..)))
    ));
  }

  #[test]
  fn lex_eof() {
    let mut lexer = Lexer::new(vec!['a']);

    assert_eq!(true, lexer.lex_token_kind().is_ok());
    assert!(lexer.is_eof());
  }

  #[test]
  fn lex_empty() {
    let lexer = Lexer::new(Vec::default());

    assert!(lexer.is_eof());
  }

  #[test]
  fn lex_illegal() {
    let mut lexer = Lexer::new(vec!['\\', '^']);

    assert!(matches!(
      lexer.lex_token_kind(),
      Ok(Some(TokenKind::Illegal('\\')))
    ));

    assert!(matches!(
      lexer.lex_token_kind(),
      Ok(Some(TokenKind::Illegal('^')))
    ));
  }

  #[test]
  fn read_char_empty() {
    let mut lexer = Lexer::new(Vec::default());

    assert_eq!(None, lexer.read_char());
  }

  #[test]
  fn read_char_single() {
    let mut lexer = Lexer::new(vec!['a', 'b']);

    assert_eq!(Some('a'), lexer.current_char);
    assert_eq!(Some('b'), lexer.read_char());
  }

  #[test]
  fn read_char_overflow() {
    let mut lexer = Lexer::new(vec!['a']);

    lexer.read_char();
    assert_eq!(0, lexer.index);
    assert_eq!(None, lexer.current_char);
  }

  #[test]
  fn is_whitespace() {
    assert_eq!(true, Lexer::is_whitespace(' '));
    assert_eq!(false, Lexer::is_whitespace('a'));
  }

  #[test]
  fn lex_comment() {
    let mut lexer = Lexer::from("--test");

    assert_sequence(&mut lexer, &[TokenKind::Comment("test".to_string())]);
  }

  #[test]
  fn lex_comment_space() {
    let mut lexer = Lexer::from("-- hello world");

    assert_sequence(
      &mut lexer,
      &[TokenKind::Comment(" hello world".to_string())],
    );
  }

  #[test]
  fn lex_comment_new_line() {
    let mut lexer = Lexer::from("--hello\n world");

    assert_sequence(&mut lexer, &[TokenKind::Comment("hello".to_string())]);
  }

  #[test]
  fn lex_comment_before() {
    let mut lexer = Lexer::from("a--hello\n world");

    assert_eq!(true, lexer.lex_token_kind().is_ok());
    assert_sequence(&mut lexer, &[TokenKind::Comment("hello".to_string())]);
  }

  #[test]
  fn lex_string() {
    let mut lexer = Lexer::from("\"hello\"");

    assert_sequence(&mut lexer, &[TokenKind::String("hello".to_string())]);
  }

  #[test]
  fn lex_number_single_digit() {
    let mut lexer = Lexer::from("1");

    assert_sequence(&mut lexer, &[TokenKind::Number(1 as f64, false)]);
  }

  #[test]
  fn lex_number() {
    let mut lexer = Lexer::from("123");

    assert_sequence(&mut lexer, &[TokenKind::Number(123 as f64, false)]);
  }

  #[test]
  fn lex_indent() {
    let mut lexer = Lexer::from("  a  \n  b");

    let expected_tokens = &[
      TokenKind::Indent,
      TokenKind::Identifier(String::from("a")),
      TokenKind::Whitespace(' '),
      TokenKind::Whitespace(' '),
      TokenKind::Whitespace('\n'),
      TokenKind::Identifier(String::from("b")),
    ];

    assert_sequence(&mut lexer, expected_tokens);
  }

  #[test]
  fn lex_dedent() {
    let mut lexer = Lexer::from("  a\nb");

    let expected_tokens = &[
      TokenKind::Indent,
      TokenKind::Identifier("a".to_string()),
      TokenKind::Whitespace('\n'),
      TokenKind::Dedent,
      TokenKind::Identifier("b".to_string()),
    ];

    assert_sequence(&mut lexer, expected_tokens);
  }

  #[test]
  fn lex_dedent_eof() {
    let mut lexer = Lexer::from("  a\n    b\n");

    let expected_tokens = &[
      TokenKind::Indent,
      TokenKind::Identifier("a".to_string()),
      TokenKind::Whitespace('\n'),
      TokenKind::Indent,
      TokenKind::Identifier("b".to_string()),
      TokenKind::Whitespace('\n'),
      TokenKind::Dedent,
      TokenKind::Dedent,
    ];

    assert_sequence(&mut lexer, expected_tokens);
  }

  // TODO: Write test to ensure that dedents slowly decent (such as the case with `impl`). The change that was used to "fix" this was the decremental of the indentation level, instead of simply setting it equal to the indent counter.

  #[test]
  fn lex_all() {
    let result = Lexer::lex_all("let one = 1").expect("lexing should succeed");

    assert_eq!(7, result.len());
  }

  #[test]
  fn read_while() {
    const TEST_INPUT: &str = "123456";

    let mut lexer = Lexer::from(TEST_INPUT);
    let result = lexer.read_while(|character| character.is_digit(10));

    assert_eq!(result, TEST_INPUT);
  }

  // TODO: Need test to catch bug with comments messing up indentation.
  // TODO: Add tests for number-overflow cases.
  // TODO: Add test(s) to ensure that comments don't affect indentation levels.
}
