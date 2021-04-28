module Cherry.Data.Expression.Identifier exposing
    ( Identifier(..)
    , decoder
    , parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.Expression.Operator as Operator exposing (Operator)
import Cherry.Data.Keywords as Keywords
import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))
import Set


-- TYPES -----------------------------------------------------------------------


{-| -}
type Identifier
    = Local String
    | Scoped (List String) String
    | Operator Operator
    | Field String


-- PARSING JSON ----------------------------------------------------------------


{-| -}
decoder : Decoder Identifier
decoder =
    Json.Decode.oneOf
        [ localIdentifierDecoder
        , scopedIdentifierDecoder
        , operatorIdentifierDecoder
        , fieldIdentifierDecoder
        ]

{-| -}
localIdentifierDecoder : Decoder Identifier
localIdentifierDecoder =
    Json.Decode.Extra.taggedObject "Identifier.Local" <|
        Json.Decode.map Local
            (Json.Decode.field "name" Json.Decode.string)

{-| -}
scopedIdentifierDecoder : Decoder Identifier
scopedIdentifierDecoder =
    Json.Decode.Extra.taggedObject "Identifier.Scoped" <|
        Json.Decode.map2 Scoped
            (Json.Decode.field "namespace" <|
                Json.Decode.list Json.Decode.string
            )
            (Json.Decode.field "name" Json.Decode.string)

{-| -}
operatorIdentifierDecoder : Decoder Identifier
operatorIdentifierDecoder =
    Json.Decode.Extra.taggedObject "Identifier.Operator" <|
        Json.Decode.map Operator
            (Json.Decode.field "operator" Operator.decoder)

{-| -}
fieldIdentifierDecoder : Decoder Identifier
fieldIdentifierDecoder =
    Json.Decode.Extra.taggedObject "Identifier.Field" <|
        Json.Decode.map Field
            (Json.Decode.field "name" Json.Decode.string)


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : Parser Identifier
parser =
    Parser.oneOf
        [ localIdentifierParser
        , scopedIdentifierParser
        , operatorIdentifierParser
        , fieldIdentifierParser
        ]

{-| -}
localIdentifierParser : Parser Identifier
localIdentifierParser =
    Parser.succeed Local
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }

{-| -}
scopedIdentifierParser : Parser Identifier
scopedIdentifierParser =
    Parser.succeed Scoped
        |= Parser.sequence
            { start = ""
            , separator = "."
            , end = ""
            , item = Parser.variable
                { start = Char.isUpper
                , inner = Char.isAlphaNum
                , reserved = Set.empty
                }
            , spaces = Parser.succeed ()
            , trailing = Parser.Mandatory
            }
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }

{-| -}
operatorIdentifierParser : Parser Identifier
operatorIdentifierParser =
    Parser.succeed Operator
        |. Parser.symbol "("
        |= Parser.oneOf
            [ Parser.succeed Operator.Pipe    |. Parser.symbol "|>"
            , Parser.succeed Operator.Compose |. Parser.symbol ">>"
            , Parser.succeed Operator.Eq      |. Parser.symbol "=="
            , Parser.succeed Operator.NotEq   |. Parser.symbol "!="
            , Parser.succeed Operator.Lte     |. Parser.symbol "<="
            , Parser.succeed Operator.Gte     |. Parser.symbol ">="
            , Parser.succeed Operator.And     |. Parser.symbol "&&"
            , Parser.succeed Operator.Or      |. Parser.symbol "||"
            , Parser.succeed Operator.Cons    |. Parser.symbol "::"
            , Parser.succeed Operator.Join    |. Parser.symbol "++"
            , Parser.succeed Operator.Discard |. Parser.symbol ";"
            , Parser.succeed Operator.Lt      |. Parser.symbol "<"
            , Parser.succeed Operator.Gt      |. Parser.symbol ">"
            , Parser.succeed Operator.Add     |. Parser.symbol "+"
            , Parser.succeed Operator.Sub     |. Parser.symbol "-"
            , Parser.succeed Operator.Mul     |. Parser.symbol "*"
            , Parser.succeed Operator.Div     |. Parser.symbol "/"
            , Parser.succeed Operator.Pow     |. Parser.symbol "^"
            , Parser.succeed Operator.Mod     |. Parser.symbol "%"
            ]
        |. Parser.symbol ")"

{-| -}
fieldIdentifierParser : Parser Identifier
fieldIdentifierParser =
    Parser.succeed Field
        |. Parser.symbol "."
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }

