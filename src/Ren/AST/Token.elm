module Ren.AST.Token exposing (..)

{-| -}

import Ren.AST.Expr as Expr
import Ren.Data.Span exposing (Span)
import Set exposing (Set)



-- IMPORTS ---------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------


{-| -}
type Token
    = Identifier String --  id | Hello | ...
    | Keyword Keyword --     let | import | where | ...
    | Symbol Symbol --      ( | } | [ | ...
    | Operator Expr.Operator --    * | |> | :: | ...
    | Comment String --     // this is a comment
    | Boolean Bool --       true | false
    | Number Float --       1 | 0.4 | 2e6 | ...
    | String String --      "I am a string"
    | EOF
    | Unknown String


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
    | Ret
    | Run
    | Then
    | Where


{-| -}
type Symbol
    = Colon
    | Equal
    | LBrace
    | LBracket
    | LParen
    | Period
    | RBrace
    | RBracket
    | RParen



-- CONSTANTS -------------------------------------------------------------------


keywords : Set String
keywords =
    Set.fromList <|
        List.concat
            -- Imports
            [ [ "import", "pkg", "ext", "exposing", "as" ]

            -- Declarations
            , [ "pub", "type", "let", "ext", "run" ]

            -- Conditionals
            , [ "if", "then", "else" ]

            -- Pattern matching
            , [ "where", "is", "if" ]

            -- Blocks
            , [ "ret" ]
            ]


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
            , [ ":", "." ]

            --
            , [ "=" ]
            ]


operators : Set String
operators =
    Set.fromList <|
        List.concat
            --
            [ [ "|>", ">>" ]

            --
            , [ "+", "-", "*", "/", "%", "^" ]

            --
            , [ "==", "!=", ">", ">=", "<", "<=" ]

            --
            , [ "&&", "||" ]

            --
            , [ "::", "++" ]
            ]



-- CONSTRUCTORS ----------------------------------------------------------------


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

        "ret" ->
            Just <| Keyword Ret

        "run" ->
            Just <| Keyword Run

        "then" ->
            Just <| Keyword Then

        "where" ->
            Just <| Keyword Where

        _ ->
            Nothing


symbol : String -> Maybe Token
symbol s =
    case s of
        ":" ->
            Just <| Symbol Colon

        "=" ->
            Just <| Symbol Equal

        "{" ->
            Just <| Symbol LBrace

        "[" ->
            Just <| Symbol LBracket

        "(" ->
            Just <| Symbol LParen

        "." ->
            Just <| Symbol Period

        "}" ->
            Just <| Symbol RBrace

        "]" ->
            Just <| Symbol RBracket

        ")" ->
            Just <| Symbol RParen

        _ ->
            Nothing


operator : String -> Maybe Token
operator s =
    case s of
        "|>" ->
            Just <| Operator Expr.Pipe

        ">>" ->
            Just <| Operator Expr.Compose

        "+" ->
            Just <| Operator Expr.Add

        "-" ->
            Just <| Operator Expr.Sub

        "*" ->
            Just <| Operator Expr.Mul

        "/" ->
            Just <| Operator Expr.Div

        "^" ->
            Just <| Operator Expr.Pow

        "%" ->
            Just <| Operator Expr.Mod

        "==" ->
            Just <| Operator Expr.Eq

        "!=" ->
            Just <| Operator Expr.NotEq

        "<" ->
            Just <| Operator Expr.Lt

        "<=" ->
            Just <| Operator Expr.Lte

        ">" ->
            Just <| Operator Expr.Gt

        ">=" ->
            Just <| Operator Expr.Gte

        "&&" ->
            Just <| Operator Expr.And

        "||" ->
            Just <| Operator Expr.Or

        "::" ->
            Just <| Operator Expr.Cons

        "++" ->
            Just <| Operator Expr.Join

        _ ->
            Nothing



-- QUERIES ---------------------------------------------------------------------
-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------
