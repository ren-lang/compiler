module Ren.Stage.Parse exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Ast.Core as Core
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Control.Parser as Parser exposing (Parser, drop, keep)
import Ren.Control.Parser.Pratt as Pratt
import Ren.Data.Token as Token exposing (Token)



--


parse stream =
    stream
        |> Parser.run expr
        |> Result.mapError (\_ -> ())



-- TYPES -----------------------------------------------------------------------
-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------
-- QUERIES ---------------------------------------------------------------------
-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------
-- PARSERS ---------------------------------------------------------------------


expr : Parser () () Expr
expr =
    pratt
        [ parenthesised
        , lambda
        , Pratt.literal <| literal Expr.Literal Expr.Var <| Parser.lazy (\_ -> expr)
        ]


pratt : List (Pratt.Parsers () () Expr -> Parser () () Expr) -> Parser () () Expr
pratt parsers =
    Pratt.expression
        { oneOf = parsers
        , andThenOneOf = operators
        }


operators : List (Pratt.Operator () () Expr)
operators =
    let
        operator ( assoc, precedence, sym ) =
            assoc precedence (Parser.operator () sym) (Expr.Binop sym)
    in
    List.map operator
        [ ( Pratt.infixLeft, 1, Expr.Pipe )
        , ( Pratt.infixLeft, 4, Expr.Eq )
        , ( Pratt.infixLeft, 4, Expr.Gt )
        , ( Pratt.infixLeft, 4, Expr.Gte )
        , ( Pratt.infixLeft, 4, Expr.Lt )
        , ( Pratt.infixLeft, 4, Expr.Lte )
        , ( Pratt.infixLeft, 4, Expr.Neq )
        , ( Pratt.infixLeft, 6, Expr.Add )
        , ( Pratt.infixLeft, 6, Expr.Sub )
        , ( Pratt.infixLeft, 7, Expr.Div )
        , ( Pratt.infixLeft, 7, Expr.Mul )
        , ( Pratt.infixRight, 2, Expr.Or )
        , ( Pratt.infixRight, 3, Expr.And )
        , ( Pratt.infixRight, 5, Expr.Concat )
        , ( Pratt.infixRight, 5, Expr.Cons )
        ]



--


parenthesised : Pratt.Parsers () () Expr -> Parser () () Expr
parenthesised parsers =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol () <| Token.Paren Token.Left)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.drop (Parser.symbol () <| Token.Paren Token.Right)



--


lambda : Pratt.Parsers () () Expr -> Parser () () Expr
lambda parsers =
    Parser.succeed (\pat rest body -> Expr.Lambda (pat :: rest) body)
        |> Parser.drop (Parser.keyword () Token.Fun)
        |> Parser.keep (Parser.debug "pattern" pattern)
        |> Parser.keep
            (Parser.loop []
                (\pats ->
                    Parser.oneOf
                        [ Parser.succeed (\pat -> pat :: pats)
                            |> Parser.keep (Parser.debug "pattern" pattern)
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.drop (Parser.symbol () Token.FatArrow)
                            |> Parser.map (\_ -> List.reverse pats)
                            |> Parser.map Parser.Break
                        ]
                )
            )
        |> Parser.keep (Pratt.subExpression 0 parsers)



-- PARSERS: LITERALS -----------------------------------------------------------


literal : (Core.Literal a -> a) -> (String -> a) -> Parser () () a -> Parser () () a
literal wrap fromString subexpr =
    Parser.map wrap <|
        Parser.oneOf
            [ array subexpr
            , constructor subexpr
            , number
            , record fromString subexpr
            , string
            ]


array : Parser () () a -> Parser () () (Core.Literal a)
array subexpr =
    let
        elements =
            Parser.loop []
                (\els ->
                    Parser.oneOf
                        [ Parser.succeed (\el -> el :: els)
                            |> Parser.drop (Parser.symbol () <| Token.Comma)
                            |> Parser.keep subexpr
                            |> Parser.map Parser.Continue
                        , Parser.succeed (\_ -> List.reverse els)
                            |> Parser.keep (Parser.symbol () <| Token.Bracket Token.Right)
                            |> Parser.map Parser.Break
                        ]
                )
    in
    Parser.succeed Core.LArr
        |> Parser.drop (Parser.symbol () <| Token.Bracket Token.Left)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.succeed (::)
                    |> Parser.keep subexpr
                    |> Parser.keep elements
                , Parser.succeed []
                    |> Parser.drop (Parser.symbol () <| Token.Bracket Token.Right)
                ]
            )


constructor : Parser () () a -> Parser () () (Core.Literal a)
constructor subexpr =
    let
        args =
            Parser.loop []
                (\xs ->
                    Parser.oneOf
                        [ Parser.succeed (\x -> x :: xs)
                            |> Parser.keep subexpr
                            |> Parser.map Parser.Continue
                        , Parser.succeed (List.reverse xs)
                            |> Parser.map Parser.Break
                        ]
                )
    in
    Parser.succeed Core.LCon
        |> Parser.drop (Parser.symbol () <| Token.Hash)
        |> Parser.keep (Parser.identifier () Token.Lower)
        |> Parser.keep args


number : Parser () () (Core.Literal a)
number =
    Parser.succeed Core.LNum
        |> Parser.keep (Parser.number ())


record : (String -> a) -> Parser () () a -> Parser () () (Core.Literal a)
record fromString subexpr =
    let
        field =
            Parser.succeed (\key val -> ( key, Maybe.withDefault (fromString key) val ))
                |> Parser.keep (Parser.identifier () Token.Lower)
                |> Parser.keep
                    (Parser.oneOf
                        [ Parser.succeed Just
                            |> Parser.drop (Parser.symbol () <| Token.Colon)
                            |> Parser.keep subexpr
                        , Parser.succeed Nothing
                        ]
                    )

        fields =
            Parser.loop []
                (\fs ->
                    Parser.oneOf
                        [ Parser.succeed (\f -> f :: fs)
                            |> Parser.drop (Parser.symbol () Token.Comma)
                            |> Parser.keep field
                            |> Parser.map Parser.Continue
                        , Parser.succeed (\_ -> List.reverse fs)
                            |> Parser.keep (Parser.symbol () <| Token.Brace Token.Right)
                            |> Parser.map Parser.Break
                        ]
                )
    in
    Parser.succeed Core.LRec
        |> Parser.drop (Parser.symbol () <| Token.Brace Token.Left)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.succeed (::)
                    |> Parser.keep field
                    |> Parser.keep fields
                , Parser.succeed []
                    |> Parser.drop (Parser.symbol () <| Token.Brace Token.Right)
                ]
            )


string : Parser () () (Core.Literal a)
string =
    Parser.succeed Core.LStr
        |> Parser.keep (Parser.string ())



-- PARSERS: PATTERNS -----------------------------------------------------------


pattern : Parser () () Core.Pattern
pattern =
    Parser.oneOf
        [ pany
        , plit
        , ptyp
        , pvar
        ]


pany : Parser () () Core.Pattern
pany =
    Parser.succeed Core.PAny
        |> Parser.drop (Parser.symbol () Token.Underscore)


plit : Parser () () Core.Pattern
plit =
    literal Core.PLit Core.PVar <| Parser.lazy (\_ -> pattern)


ptyp : Parser () () Core.Pattern
ptyp =
    Parser.succeed Core.PTyp
        |> Parser.drop (Parser.symbol () Token.At)
        |> Parser.keep (Parser.identifier () Token.Upper)
        |> Parser.keep (Parser.lazy (\_ -> pattern))


pvar : Parser () () Core.Pattern
pvar =
    Parser.succeed Core.PVar
        |> Parser.keep (Parser.identifier () Token.Lower)
