module Ren.Compiler.Error exposing
    ( Error(..)
    , ParseError, ParseContext(..), expectingKeyword, expectingSymbol, expectingOperator, expectingChar, expectingNumber, expectingEOF, expectingWhitespace, expectingCamelCase, expectingCapitalCase, expectingExpr, expectingType, unexpectedChar, internalParseError
    , TypeError, infiniteType, incompatibleTypes, missingField, typeTooGeneral, arityMismatch, unknownType, internalTypeError
    , toString
    )

{-|

@docs Error
@docs ParseError, ParseContext, expectingKeyword, expectingSymbol, expectingOperator, expectingChar, expectingNumber, expectingEOF, expectingWhitespace, expectingCamelCase, expectingCapitalCase, expectingExpr, expectingType, unexpectedChar, internalParseError
@docs TypeError, infiniteType, incompatibleTypes, missingField, typeTooGeneral, arityMismatch, unknownType, internalTypeError
@docs toString

-}

-- IMPORTS ---------------------------------------------------------------------

import Parser.Advanced
import Ren.Data.Type exposing (Type)



-- TYPES -----------------------------------------------------------------------


{-| -}
type Error
    = ParseError (List (Parser.Advanced.DeadEnd ParseContext ParseError))
    | TypeError TypeError


{-| -}
type ParseError
    = ExpectingKeyword String
    | ExpectingSymbol String
    | ExpectingOperator String
    | ExpectingChar
    | ExpectingNumber
    | ExpectingEOF
    | ExpectingWhitespace
    | ExpectingCamelCase
    | ExpectingCapitalCase
    | ExpectingExpr
    | ExpectingType
    | UnexpextedChar Char
    | InternalParseError String


{-| -}
type ParseContext
    = InImport
    | InDeclaration
    | InExpr
      --
    | InIf
    | InAnnotation


{-| -}
type TypeError
    = InfiniteType Type Type
    | IncompatibleTypes Type Type
    | MissingField String
    | TypeTooGeneral Type Type
    | ArityMismatch String Int Int
    | UnknownType Type
    | InternalTypeError String



-- CONSTRUCTORS: PARSE ERRORS --------------------------------------------------


{-| -}
expectingKeyword : String -> ParseError
expectingKeyword keyword =
    ExpectingKeyword keyword


{-| -}
expectingSymbol : String -> ParseError
expectingSymbol symbol =
    ExpectingSymbol symbol


{-| -}
expectingOperator : String -> ParseError
expectingOperator operator =
    ExpectingOperator operator


{-| -}
expectingChar : ParseError
expectingChar =
    ExpectingChar


{-| -}
expectingNumber : ParseError
expectingNumber =
    ExpectingNumber


{-| -}
expectingEOF : ParseError
expectingEOF =
    ExpectingEOF


{-| -}
expectingWhitespace : ParseError
expectingWhitespace =
    ExpectingWhitespace


{-| -}
expectingCamelCase : ParseError
expectingCamelCase =
    ExpectingCamelCase


{-| -}
expectingCapitalCase : ParseError
expectingCapitalCase =
    ExpectingCapitalCase


{-| -}
expectingExpr : ParseError
expectingExpr =
    ExpectingExpr


{-| -}
expectingType : ParseError
expectingType =
    ExpectingType


{-| -}
unexpectedChar : Char -> ParseError
unexpectedChar char =
    UnexpextedChar char


{-| -}
internalParseError : String -> ParseError
internalParseError message =
    InternalParseError message



-- CONSTRUCTORS: TYPE ERRORS ---------------------------------------------------


{-| -}
infiniteType : Type -> Type -> TypeError
infiniteType t1 t2 =
    InfiniteType t1 t2


{-| -}
incompatibleTypes : Type -> Type -> TypeError
incompatibleTypes t1 t2 =
    IncompatibleTypes t1 t2


{-| -}
missingField : String -> TypeError
missingField field =
    MissingField field


{-| -}
typeTooGeneral : Type -> Type -> TypeError
typeTooGeneral t1 t2 =
    TypeTooGeneral t1 t2


arityMismatch : String -> Int -> Int -> TypeError
arityMismatch name expected got =
    ArityMismatch name expected got


{-| -}
unknownType : Type -> TypeError
unknownType t =
    UnknownType t


{-| -}
internalTypeError : String -> TypeError
internalTypeError message =
    InternalTypeError message



-- CONVERSIONS -----------------------------------------------------------------


{-| -}
toString : Error -> String
toString error =
    case error of
        ParseError es ->
            List.map parseErrorToString es
                |> String.join "\n"

        TypeError e ->
            typeErrorToString e


parseErrorToString : Parser.Advanced.DeadEnd ParseContext ParseError -> String
parseErrorToString { row, col, problem } =
    let
        location =
            "at { line: " ++ String.fromInt row ++ ", col: " ++ String.fromInt col ++ " }"
    in
    case problem of
        ExpectingKeyword keyword ->
            "ExpectingKeyword: " ++ keyword ++ " " ++ location

        ExpectingSymbol symbol ->
            "ExpectingSymbol: " ++ symbol ++ " " ++ location

        ExpectingOperator operator ->
            "ExpectingOperator: " ++ operator ++ " " ++ location

        ExpectingChar ->
            "ExpectingChar: " ++ location

        ExpectingNumber ->
            "ExpectingNumber: " ++ location

        ExpectingEOF ->
            "ExpectingEOF: " ++ location

        ExpectingWhitespace ->
            "ExpectingWhitespace: " ++ location

        ExpectingCamelCase ->
            "ExpectingCamelCase: " ++ location

        ExpectingCapitalCase ->
            "ExpectingCapitalCase: " ++ location

        ExpectingExpr ->
            "ExpectingExpr: " ++ location

        ExpectingType ->
            "ExpectingType: " ++ location

        UnexpextedChar char ->
            "UnexpextedChar: '" ++ String.fromChar char ++ "' " ++ location

        InternalParseError message ->
            "InternalParseError: " ++ message ++ " " ++ location


typeErrorToString : TypeError -> String
typeErrorToString error =
    case error of
        InfiniteType t1 t2 ->
            "InfiniteType: " ++ Ren.Data.Type.toString t1 ++ " " ++ Ren.Data.Type.toString t2

        IncompatibleTypes t1 t2 ->
            "IncompatibleTypes: " ++ Ren.Data.Type.toString t1 ++ " " ++ Ren.Data.Type.toString t2

        MissingField field ->
            "MissingField: " ++ field

        TypeTooGeneral t1 t2 ->
            "TypeTooGeneral: " ++ Ren.Data.Type.toString t1 ++ " " ++ Ren.Data.Type.toString t2

        ArityMismatch name expected got ->
            "ArityMismatch: " ++ name ++ " expected " ++ String.fromInt expected ++ " type params but got " ++ String.fromInt got

        UnknownType t ->
            "UnknownType: " ++ Ren.Data.Type.toString t

        InternalTypeError message ->
            "InternalTypeError: " ++ message
