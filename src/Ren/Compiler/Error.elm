module Ren.Compiler.Error exposing
    ( Error(..)
    , ParseError, ParseContext(..), expectingKeyword, expectingSymbol, expectingOperator, expectingTypePattern, expectingChar, expectingNumber, expectingEOF, expectingWhitespace, expectingCamelCase, expectingCapitalCase, unexpectedChar, internalParseError
    , TypeError, infiniteType, incompatibleTypes, missingField, typeTooGeneral, internalTypeError
    )

{-|

@docs Error, internalError
@docs ParseError, ParseContext, expectingKeyword, expectingSymbol, expectingOperator, expectingTypePattern, expectingChar, expectingNumber, expectingEOF, expectingWhitespace, expectingCamelCase, expectingCapitalCase, unexpectedChar, internalParseError
@docs TypeError, infiniteType, incompatibleTypes, missingField, typeTooGeneral, internalTypeError

-}

-- IMPORTS ---------------------------------------------------------------------

import Parser.Advanced
import Ren.Data.Type exposing (Type)



-- TYPES -----------------------------------------------------------------------


type Error
    = ParseError (List (Parser.Advanced.DeadEnd ParseContext ParseError))
    | TypeError TypeError


{-| -}
type ParseError
    = ExpectingKeyword String
    | ExpectingSymbol String
    | ExpectingOperator String
    | ExpectingTypePattern (List String)
    | ExpectingChar
    | ExpectingNumber
    | ExpectingEOF
    | ExpectingWhitespace
    | ExpectingCamelCase
    | ExpectingCapitalCase
    | UnexpextedChar Char
    | InternalParseError String


{-| -}
type ParseContext
    = InImport
    | InDeclaration
    | InExpr


{-| -}
type TypeError
    = InfiniteType Type Type
    | IncompatibleTypes Type Type
    | MissingField String
    | TypeTooGeneral Type Type
    | InternalTypeError String



-- CONSTRUCTORS: PARSE ERRORS --------------------------------------------------


expectingKeyword : String -> ParseError
expectingKeyword keyword =
    ExpectingKeyword keyword


expectingSymbol : String -> ParseError
expectingSymbol symbol =
    ExpectingSymbol symbol


expectingOperator : String -> ParseError
expectingOperator operator =
    ExpectingOperator operator


expectingTypePattern : List String -> ParseError
expectingTypePattern patterns =
    ExpectingTypePattern patterns


expectingChar : ParseError
expectingChar =
    ExpectingChar


expectingNumber : ParseError
expectingNumber =
    ExpectingNumber


expectingEOF : ParseError
expectingEOF =
    ExpectingEOF


expectingWhitespace : ParseError
expectingWhitespace =
    ExpectingWhitespace


expectingCamelCase : ParseError
expectingCamelCase =
    ExpectingCamelCase


expectingCapitalCase : ParseError
expectingCapitalCase =
    ExpectingCapitalCase


unexpectedChar : Char -> ParseError
unexpectedChar char =
    UnexpextedChar char


internalParseError : String -> ParseError
internalParseError message =
    InternalParseError message



-- CONSTRUCTORS: TYPE ERRORS ---------------------------------------------------


infiniteType : Type -> Type -> TypeError
infiniteType t1 t2 =
    InfiniteType t1 t2


incompatibleTypes : Type -> Type -> TypeError
incompatibleTypes t1 t2 =
    IncompatibleTypes t1 t2


missingField : String -> TypeError
missingField field =
    MissingField field


typeTooGeneral : Type -> Type -> TypeError
typeTooGeneral t1 t2 =
    TypeTooGeneral t1 t2


internalTypeError : String -> TypeError
internalTypeError message =
    InternalTypeError message
