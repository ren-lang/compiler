module Cherry.Stage.Parse.Expression.Operator exposing 
    ( table
    , asIdentifierParser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Parser exposing ((|=), (|.), Parser)
import Pratt


-- PRECEDENCE TABLE ------------------------------------------------------------


{-| -}
table : List (Pratt.Config AST.Expression -> ( Int, AST.Expression -> Parser AST.Expression ))
table =
    [ AST.Infix AST.Pipe    |> Pratt.infixLeft  1 (Parser.symbol "|>")
    , AST.Infix AST.Compose |> Pratt.infixRight 9 (Parser.symbol ">>")
    , AST.Infix AST.Eq      |> Pratt.infixLeft  4 (Parser.symbol "==")
    , AST.Infix AST.NotEq   |> Pratt.infixLeft  4 (Parser.symbol "!=")
    , AST.Infix AST.Lte     |> Pratt.infixLeft  4 (Parser.symbol "<=")
    , AST.Infix AST.Gte     |> Pratt.infixLeft  4 (Parser.symbol ">=")
    , AST.Infix AST.And     |> Pratt.infixRight 3 (Parser.symbol "&&")
    , AST.Infix AST.Or      |> Pratt.infixRight 2 (Parser.symbol "||")
    , AST.Infix AST.Cons    |> Pratt.infixRight 5 (Parser.symbol "::")
    , AST.Infix AST.Join    |> Pratt.infixRight 5 (Parser.symbol "++")

    -- ONE CHARACTER OPERATORS
    , AST.Infix AST.Discard |> Pratt.infixRight 1 (Parser.symbol ";")
    , AST.Infix AST.Lt      |> Pratt.infixLeft  4 (Parser.symbol "<")
    , AST.Infix AST.Gt      |> Pratt.infixLeft  4 (Parser.symbol ">")
    , AST.Infix AST.Add     |> Pratt.infixLeft  6 (Parser.symbol "+")
    , AST.Infix AST.Sub     |> Pratt.infixLeft  6 (Parser.symbol "-")
    , AST.Infix AST.Mul     |> Pratt.infixLeft  7 (Parser.symbol "*")
    , AST.Infix AST.Div     |> Pratt.infixLeft  7 (Parser.symbol "/")
    , AST.Infix AST.Pow     |> Pratt.infixRight 8 (Parser.symbol "^")
    , AST.Infix AST.Mod     |> Pratt.infixRight 8 (Parser.symbol "%")
    ]


-- IDENTIFIER PARSER -----------------------------------------------------------


{-| -}
asIdentifierParser : Parser AST.Identifier
asIdentifierParser =
    Parser.succeed AST.Operator
        |. Parser.symbol "("
        |= Parser.oneOf
            [ Parser.succeed AST.Pipe    |. Parser.symbol "|>"
            , Parser.succeed AST.Compose |. Parser.symbol ">>"
            , Parser.succeed AST.Eq      |. Parser.symbol "=="
            , Parser.succeed AST.NotEq   |. Parser.symbol "!="
            , Parser.succeed AST.Lte     |. Parser.symbol "<="
            , Parser.succeed AST.Gte     |. Parser.symbol ">="
            , Parser.succeed AST.And     |. Parser.symbol "&&"
            , Parser.succeed AST.Or      |. Parser.symbol "||"
            , Parser.succeed AST.Cons    |. Parser.symbol "::"
            , Parser.succeed AST.Join    |. Parser.symbol "++"
            , Parser.succeed AST.Discard |. Parser.symbol ";"
            , Parser.succeed AST.Lt      |. Parser.symbol "<"
            , Parser.succeed AST.Gt      |. Parser.symbol ">"
            , Parser.succeed AST.Add     |. Parser.symbol "+"
            , Parser.succeed AST.Sub     |. Parser.symbol "-"
            , Parser.succeed AST.Mul     |. Parser.symbol "*"
            , Parser.succeed AST.Div     |. Parser.symbol "/"
            , Parser.succeed AST.Pow     |. Parser.symbol "^"
            , Parser.succeed AST.Mod     |. Parser.symbol "%"
            ]
        |. Parser.symbol ")"
