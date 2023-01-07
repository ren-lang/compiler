// IMPORTS ---------------------------------------------------------------------

import ren/data/token.{Token}

// TYPES -----------------------------------------------------------------------

///
///
pub type Error {
  InternalError(String)
  MultipleErrors(List(Error))
  ParseError(ParseError)
}

pub type ParseError {
  Expected(Token, got: Token)
  ExpectedLowerIdentifier(got: Token)
  ExpectedNumber(got: Token)
  ExpectedString(got: Token)
  ExpectedUpperIdentifier(got: Token)
  //
  UnexpectedEOF
  UnexpectedInput
  //
  InternalParseError(String)
}
