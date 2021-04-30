module Ren.Data.Expression.Pattern exposing 
    ( Pattern(..)
    , decoder
    , parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))
import Ren.Data.Expression.Literal as Literal exposing (Literal)
import Ren.Data.Keywords as Keywords


-- TYPES -----------------------------------------------------------------------


{-| -}
type Pattern expression
    = ArrayDestructure (List (Pattern expression))
    | Name String
    | ObjectDestructure (List ( String, Maybe (Pattern expression) ))
    | Value (Literal expression)
    | Wildcard (Maybe String)


-- PARSING JSON ----------------------------------------------------------------


{-| -}
decoder : Decoder (Pattern expression)
decoder  =
    Json.Decode.oneOf
        [ arrayDestructureDecoder
        , nameDecoder
        , objectDestructureDecoder
        , valueDecoder
        , wildcardDecoder
        ]

{-| -}
lazyDecoder : Decoder (Pattern expression)
lazyDecoder =
    Json.Decode.lazy (\_ -> decoder)

{-| -}
arrayDestructureDecoder : Decoder (Pattern expression)
arrayDestructureDecoder =
    Json.Decode.Extra.taggedObject "Pattern.ArrayDestructure" <|
        Json.Decode.map ArrayDestructure
            (Json.Decode.field "patterns" <|
                Json.Decode.list lazyDecoder
            )

{-| -}
nameDecoder : Decoder (Pattern expression)
nameDecoder =
    Json.Decode.Extra.taggedObject "Pattern.Name" <|
        Json.Decode.map Name
            (Json.Decode.field "name" Json.Decode.string)

{-| -}
objectDestructureDecoder : Decoder (Pattern expression)
objectDestructureDecoder =
    Json.Decode.Extra.taggedObject "Pattern.ObjectDestructure" <|
        Json.Decode.map ObjectDestructure
            (Json.Decode.field "patterns" <|
                Json.Decode.list <|
                    Json.Decode.map2 Tuple.pair
                        (Json.Decode.field "key" Json.Decode.string)
                        (Json.Decode.field "val" lazyDecoder
                            |> Json.Decode.maybe
                        )
            )

{-| -}
valueDecoder : Decoder (Pattern expression)
valueDecoder =
    Json.Decode.Extra.taggedObject "Pattern.Value" <|
        Json.Decode.map Value
            (Json.Decode.field "value" Literal.primitiveDecoder)

{-| -}
wildcardDecoder : Decoder (Pattern expression)
wildcardDecoder =
    Json.Decode.Extra.taggedObject "Pattern.Wildcard" <|
        Json.Decode.map Wildcard
            (Json.Decode.field "name" Json.Decode.string
                |> Json.Decode.maybe
            )


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : Parser (Pattern expression)
parser =
    Parser.oneOf
        [ arrayDestructureParser
        , nameParser
        , objectDestructureParser
        , valueParser
        , wildcardParser
        ]

{-| -}
lazyParser : Parser (Pattern expression)
lazyParser =
    Parser.lazy (\_ -> parser)

{-| -}
arrayDestructureParser : Parser (Pattern expression)
arrayDestructureParser =
    Parser.succeed ArrayDestructure
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item = lazyParser
            , trailing = Parser.Forbidden
            }

{-| -}
nameParser : Parser (Pattern expression)
nameParser =
    Parser.succeed Name
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }

{-| -}
objectDestructureParser : Parser (Pattern expression)
objectDestructureParser =
    Parser.succeed ObjectDestructure
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item =
                Parser.succeed Tuple.pair
                    |= Parser.variable
                        { start = Char.isLower
                        , inner = Char.isAlphaNum
                        , reserved = Keywords.all
                        }
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.succeed Just
                            |. Parser.symbol ":"
                            |. Parser.spaces
                            |= lazyParser
                        , Parser.succeed Nothing
                        ]
            , trailing = Parser.Forbidden
            }

{-| -}
valueParser : Parser (Pattern expression)
valueParser =
    Parser.succeed Value
        |= Literal.primitiveParser

{-| -}
wildcardParser : Parser (Pattern expression)
wildcardParser =
    Parser.succeed Wildcard
        |. Parser.symbol "_"
        |= Parser.oneOf
            [ Parser.succeed Just
                |= Parser.variable
                    { start = Char.isLower
                    , inner = Char.isAlphaNum
                    , reserved = Keywords.all
                    }
            , Parser.succeed Nothing
            ]
