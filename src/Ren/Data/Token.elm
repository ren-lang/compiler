module Ren.Data.Token exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------
--

import Json.Decode
import Json.Encode
import Ren.Ast.Expr.Op as Op exposing (Op)
import Set exposing (Set)
import String
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


{-| -}
type Token
    = Comment String
    | EOF
    | Identifier Case String
    | Keyword Keyword
    | Number Float
    | Operator Op
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
    | Fun
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
    = At
    | Colon
    | Comma
    | Equal
    | FatArrow
    | Hash
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

            -- Decs
            , [ "pub", "type", "let", "ext", "do" ]

            --
            , [ "fun" ]

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
            , [ "@", "#", "_" ]
            ]


{-| -}
operators : Set String
operators =
    Set.fromList Op.symbols



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

        "fun" ->
            Just <| Keyword Fun

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

        "@" ->
            Just <| Symbol At

        "#" ->
            Just <| Symbol Hash

        "_" ->
            Just <| Symbol Underscore

        _ ->
            Nothing


{-| -}
operator : String -> Maybe Token
operator s =
    Op.fromSymbol s
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


toJson : Token -> String
toJson =
    encode >> Json.Encode.encode 4



-- JSON ------------------------------------------------------------------------


encode : Token -> Json.Encode.Value
encode token =
    case token of
        Comment comment ->
            Json.taggedEncoder "Comment"
                []
                [ Json.Encode.string comment ]

        EOF ->
            Json.taggedEncoder "EOF" [] []

        Identifier case_ id ->
            Json.taggedEncoder "Identifier"
                []
                [ Json.Encode.string <|
                    if case_ == Upper then
                        "Upper"

                    else
                        "Lower"
                , Json.Encode.string id
                ]

        Keyword kwd ->
            Json.taggedEncoder "Keyword"
                []
                [ encodeKeyword kwd
                ]

        Number n ->
            Json.taggedEncoder "Number"
                []
                [ Json.Encode.float n
                ]

        Operator op ->
            Json.taggedEncoder "Operator"
                []
                [ Op.encode op
                ]

        String s ->
            Json.taggedEncoder "String"
                []
                [ Json.Encode.string s
                ]

        Symbol sym ->
            Json.taggedEncoder "Symbol"
                []
                [ encodeSymbol sym
                ]

        Unknown unknown ->
            Json.taggedEncoder "Unknown"
                []
                [ Json.Encode.string unknown ]


encodeKeyword : Keyword -> Json.Encode.Value
encodeKeyword kwd =
    Json.Encode.string <|
        case kwd of
            As ->
                "As"

            Else ->
                "Else"

            Exposing ->
                "Exposing"

            Ext ->
                "Ext"

            Fun ->
                "Fun"

            If ->
                "If"

            Import ->
                "Import"

            Is ->
                "Is"

            Let ->
                "Let"

            Pkg ->
                "Pkg"

            Pub ->
                "Pub"

            Then ->
                "Then"

            Where ->
                "Where"


encodeSymbol : Symbol -> Json.Encode.Value
encodeSymbol sym =
    Json.Encode.string <|
        case sym of
            At ->
                "@"

            Colon ->
                ":"

            Comma ->
                ","

            Equal ->
                "="

            FatArrow ->
                "=>"

            Hash ->
                "#"

            Brace Left ->
                "{"

            Brace Right ->
                "}"

            Bracket Left ->
                "["

            Bracket Right ->
                "]"

            Paren Left ->
                "("

            Paren Right ->
                ")"

            Period ->
                "."

            Semicolon ->
                ";"

            Underscore ->
                "_"



-- UTILS -----------------------------------------------------------------------
