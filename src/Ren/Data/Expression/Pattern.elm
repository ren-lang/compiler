module Ren.Data.Expression.Pattern exposing
    ( Pattern(..)
    , decoder
    , parser
    )

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing ((|.), (|=), Parser)
import Ren.Data.Expression.Literal as Literal exposing (Literal)
import Ren.Data.Keywords as Keywords



-- TYPES -----------------------------------------------------------------------


{-| -}
type Pattern
    = ArrayDestructure (List Pattern)
    | Name String
    | ObjectDestructure (List ( String, Maybe Pattern ))
      -- What is this `Never`? The Literal type is parameterised to cover object
      -- and array literals that can contain other expressions. By using `Never`
      -- we tell Elm's type system that we can *never* have those types of literals
      -- in a Value pattern. In other words, we only support primitive literals like
      -- strings and numbers.
      -- Pretty nifty trick, eh!
    | Value (Literal Never)
    | Wildcard (Maybe String)



-- PARSING JSON ----------------------------------------------------------------


{-| -}
decoder : Decoder Pattern
decoder =
    Json.Decode.oneOf
        [ arrayDestructureDecoder
        , nameDecoder
        , objectDestructureDecoder
        , valueDecoder
        , wildcardDecoder
        ]


{-| -}
lazyDecoder : Decoder Pattern
lazyDecoder =
    Json.Decode.lazy (\_ -> decoder)


{-| -}
arrayDestructureDecoder : Decoder Pattern
arrayDestructureDecoder =
    Json.Decode.Extra.taggedObject "Pattern.ArrayDestructure" <|
        Json.Decode.map ArrayDestructure
            (Json.Decode.field "patterns" <|
                Json.Decode.list lazyDecoder
            )


{-| -}
nameDecoder : Decoder Pattern
nameDecoder =
    Json.Decode.Extra.taggedObject "Pattern.Name" <|
        Json.Decode.map Name
            (Json.Decode.field "name" Json.Decode.string)


{-| -}
objectDestructureDecoder : Decoder Pattern
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
valueDecoder : Decoder Pattern
valueDecoder =
    Json.Decode.Extra.taggedObject "Pattern.Value" <|
        Json.Decode.map Value
            (Json.Decode.field "value" Literal.primitiveDecoder)


{-| -}
wildcardDecoder : Decoder Pattern
wildcardDecoder =
    Json.Decode.Extra.taggedObject "Pattern.Wildcard" <|
        Json.Decode.map Wildcard
            (Json.Decode.field "name" Json.Decode.string
                |> Json.Decode.maybe
            )



-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : Parser Pattern
parser =
    Parser.oneOf
        [ arrayDestructureParser
        , nameParser
        , objectDestructureParser
        , valueParser
        , wildcardParser
        ]


{-| -}
lazyParser : Parser Pattern
lazyParser =
    Parser.lazy (\_ -> parser)


{-| -}
arrayDestructureParser : Parser Pattern
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
nameParser : Parser Pattern
nameParser =
    Parser.succeed Name
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }


{-| -}
objectDestructureParser : Parser Pattern
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
valueParser : Parser Pattern
valueParser =
    Parser.succeed Value
        |= Literal.primitiveParser


{-| -}
wildcardParser : Parser Pattern
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
