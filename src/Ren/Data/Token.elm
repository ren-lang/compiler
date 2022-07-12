module Ren.Data.Token exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------
--

import Ren.Ast.Expr as Expr
import Set exposing (Set)
import String



-- TYPES -----------------------------------------------------------------------


type alias Stream =
    List Token


{-| -}
type Token
    = Boolean Bool
    | Comment String
    | EOF
    | Identifier Case String
    | Keyword Keyword
    | Number Float
    | Operator Expr.Operator
    | String String
    | Symbol Symbol
    | Unknown String


{-| -}
type Case
    = Upper
    | Lower


{-| -}
type Keyword
    = As
    | Else
    | Exposing
    | Ext
    | If
    | Import
    | Is
    | Let
    | Pkg
    | Pub
    | Then
    | Where


{-| -}
type Symbol
    = Colon
    | Comma
    | Equal
    | FatArrow
    | Brace Direction
    | Bracket Direction
    | Paren Direction
    | Period
    | Semicolon
    | Underscore


{-| -}
type Direction
    = Left
    | Right



-- CONSTANTS -------------------------------------------------------------------


{-| -}
keywords : Set String
keywords =
    Set.fromList <|
        List.concat
            -- Imports
            [ [ "import", "pkg", "ext", "exposing", "as" ]

            -- Declarations
            , [ "pub", "type", "let", "ext", "do" ]

            -- Conditionals
            , [ "if", "then", "else" ]

            -- Pattern matching
            , [ "where", "is", "if" ]
            ]


{-| -}
symbols : Set String
symbols =
    Set.fromList <|
        List.concat
            -- Parens
            [ [ "(", ")" ]

            -- Braces
            , [ "{", "}" ]

            -- Brackets
            , [ "[", "]" ]

            --
            , [ ":", ";" ]

            --
            , [ ".", "," ]

            --
            , [ "=", "=>" ]

            --
            , [ "_" ]
            ]


{-| -}
operators : Set String
operators =
    Set.fromList Expr.operatorSymbols



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
identifier : String -> Maybe Token
identifier s =
    if isUpperIdentifier s then
        Just <| Identifier Upper s

    else if isLowerIdentifier s then
        Just <| Identifier Lower s

    else
        Nothing


{-| -}
keyword : String -> Maybe Token
keyword s =
    case s of
        "as" ->
            Just <| Keyword As

        "else" ->
            Just <| Keyword Else

        "exposing" ->
            Just <| Keyword Exposing

        "ext" ->
            Just <| Keyword Ext

        "if" ->
            Just <| Keyword If

        "import" ->
            Just <| Keyword Import

        "is" ->
            Just <| Keyword Is

        "let" ->
            Just <| Keyword Let

        "pkg" ->
            Just <| Keyword Pkg

        "pub" ->
            Just <| Keyword Pub

        "then" ->
            Just <| Keyword Then

        "where" ->
            Just <| Keyword Where

        _ ->
            Nothing


{-| -}
symbol : String -> Maybe Token
symbol s =
    case s of
        "(" ->
            Just <| Symbol <| Paren Left

        ")" ->
            Just <| Symbol <| Paren Right

        "{" ->
            Just <| Symbol <| Brace Left

        "}" ->
            Just <| Symbol <| Brace Right

        "[" ->
            Just <| Symbol <| Bracket Left

        "]" ->
            Just <| Symbol <| Bracket Right

        ":" ->
            Just <| Symbol Colon

        ";" ->
            Just <| Symbol Semicolon

        "." ->
            Just <| Symbol Period

        "," ->
            Just <| Symbol Comma

        "=" ->
            Just <| Symbol Equal

        "=>" ->
            Just <| Symbol FatArrow

        "_" ->
            Just <| Symbol Underscore

        _ ->
            Nothing


{-| -}
operator : String -> Maybe Token
operator s =
    Expr.operatorFromSymbol s
        |> Maybe.map Operator



-- QUERIES ---------------------------------------------------------------------


{-| -}
isIdentifier : String -> Bool
isIdentifier s =
    if List.any ((|>) s) [ isKeyword, isSymbol, isOperator ] then
        False

    else
        isUpperIdentifier s || isLowerIdentifier s


{-| -}
isUpperIdentifier : String -> Bool
isUpperIdentifier s =
    let
        isValidStart c =
            Char.isUpper c

        isValidInner c =
            Char.isAlphaNum c || c == '_'
    in
    case String.uncons s of
        Just ( char, "" ) ->
            isValidStart char

        Just ( char, rest ) ->
            isValidStart char && String.all isValidInner rest

        Nothing ->
            False


{-| -}
isLowerIdentifier : String -> Bool
isLowerIdentifier s =
    let
        isValidStart c =
            Char.isLower c

        isValidInner c =
            Char.isLower c || Char.isDigit c || c == '_'
    in
    case String.uncons s of
        Just ( char, "" ) ->
            isValidStart char

        Just ( char, rest ) ->
            isValidStart char && String.all isValidInner rest

        Nothing ->
            False


{-| -}
isKeyword : String -> Bool
isKeyword s =
    Set.member s keywords


{-| -}
isSymbol : String -> Bool
isSymbol s =
    Set.member s symbols


{-| -}
isOperator : String -> Bool
isOperator s =
    Set.member s operators



-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------
