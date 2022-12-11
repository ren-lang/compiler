// IMPORTS ---------------------------------------------------------------------

import ren/data/token.{Token}

// TYPES -----------------------------------------------------------------------

///
///
pub type Error {
  InternalError(String)
  ParseError(String)
}

pub type ParseError {
  UnexpectedToken(Token, expected: Token)
}
