module Cherry.Data.Expression.Operator exposing 
    ( Operator(..)
    , decoder
    , parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))
import Pratt


-- TYPES -----------------------------------------------------------------------

{-| -}
type Operator
    = Pipe | Compose | Discard
    -- MATHS
    | Add | Sub | Mul | Div | Pow | Mod
    -- COMPARISON
    | Eq | NotEq | Lt | Lte | Gt | Gte
    -- LOGIC
    | And | Or
    -- ARRAYS
    | Cons | Join


-- PARSING JSON ----------------------------------------------------------------


{-| -}
decoder : Decoder Operator
decoder =
    Json.Decode.oneOf
        [ Json.Decode.Extra.taggedObject "Operator.Pipe" <|
            Json.Decode.succeed Pipe
        , Json.Decode.Extra.taggedObject "Operator.Compose" <|
            Json.Decode.succeed Compose
        , Json.Decode.Extra.taggedObject "Operator.Discard" <|
            Json.Decode.succeed Discard
        -- MATHS
        , Json.Decode.Extra.taggedObject "Operator.Add" <|
            Json.Decode.succeed Add
        , Json.Decode.Extra.taggedObject "Operator.Sub" <|
            Json.Decode.succeed Sub
        , Json.Decode.Extra.taggedObject "Operator.Mul" <|
            Json.Decode.succeed Mul
        , Json.Decode.Extra.taggedObject "Operator.Div" <|
            Json.Decode.succeed Div
        , Json.Decode.Extra.taggedObject "Operator.Pow" <|
            Json.Decode.succeed Pow
        , Json.Decode.Extra.taggedObject "Operator.Mod" <|
            Json.Decode.succeed Mod
        -- COMPARISON
        , Json.Decode.Extra.taggedObject "Operator.Eq" <|
            Json.Decode.succeed Eq
        , Json.Decode.Extra.taggedObject "Operator.NotEq" <|
            Json.Decode.succeed NotEq
        , Json.Decode.Extra.taggedObject "Operator.Lt" <|
            Json.Decode.succeed Lt
        , Json.Decode.Extra.taggedObject "Operator.Lte" <|
            Json.Decode.succeed Lte
        , Json.Decode.Extra.taggedObject "Operator.Gt" <|
            Json.Decode.succeed Gt
        , Json.Decode.Extra.taggedObject "Operator.Gte" <|
            Json.Decode.succeed Gte
        -- LOGIC
        , Json.Decode.Extra.taggedObject "Operator.And" <|
            Json.Decode.succeed And
        , Json.Decode.Extra.taggedObject "Operator.Or" <|
            Json.Decode.succeed Or
        -- ARRAYS
        , Json.Decode.Extra.taggedObject "Operator.Cons" <|
            Json.Decode.succeed Cons
        , Json.Decode.Extra.taggedObject "Operator.Join" <|
            Json.Decode.succeed Join
        ]


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : (Operator -> expression -> expression -> expression) -> List (Pratt.Config expression -> ( Int, expression -> Parser expression ))
parser toExpression =
    [ toExpression Pipe    |> Pratt.infixLeft  1 (Parser.symbol "|>")
    , toExpression Compose |> Pratt.infixRight 9 (Parser.symbol ">>")
    , toExpression Eq      |> Pratt.infixLeft  4 (Parser.symbol "==")
    , toExpression NotEq   |> Pratt.infixLeft  4 (Parser.symbol "!=")
    , toExpression Lte     |> Pratt.infixLeft  4 (Parser.symbol "<=")
    , toExpression Gte     |> Pratt.infixLeft  4 (Parser.symbol ">=")
    , toExpression And     |> Pratt.infixRight 3 (Parser.symbol "&&")
    , toExpression Or      |> Pratt.infixRight 2 (Parser.symbol "||")
    , toExpression Cons    |> Pratt.infixRight 5 (Parser.symbol "::")
    , toExpression Join    |> Pratt.infixRight 5 (Parser.symbol "++")

    -- ONE CHARACTER OPERATORS
    , toExpression Discard |> Pratt.infixRight 1 (Parser.symbol ";")
    , toExpression Lt      |> Pratt.infixLeft  4 (Parser.symbol "<")
    , toExpression Gt      |> Pratt.infixLeft  4 (Parser.symbol ">")
    , toExpression Add     |> Pratt.infixLeft  6 (Parser.symbol "+")
    , toExpression Sub     |> Pratt.infixLeft  6 (Parser.symbol "-")
    , toExpression Mul     |> Pratt.infixLeft  7 (Parser.symbol "*")
    , toExpression Div     |> Pratt.infixLeft  7 (Parser.symbol "/")
    , toExpression Pow     |> Pratt.infixRight 8 (Parser.symbol "^")
    , toExpression Mod     |> Pratt.infixRight 8 (Parser.symbol "%")
    ]