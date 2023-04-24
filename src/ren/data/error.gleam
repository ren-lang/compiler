// IMPORTS ---------------------------------------------------------------------

import ren/data/token.{TokenT}

// TYPES -----------------------------------------------------------------------

///
///
pub type Error {
  InternalError(String)
  MultipleErrors(List(Error))
  ParseError(ParseError)
}

pub type ParseError {
  Expected(TokenT, got: TokenT)
  ExpectedLowerIdentifier(got: TokenT)
  ExpectedNumber(got: TokenT)
  ExpectedString(got: TokenT)
  ExpectedUpperIdentifier(got: TokenT)
  //
  UnexpectedEOF
  UnexpectedInput
  //
  InternalParseError(String)
}
