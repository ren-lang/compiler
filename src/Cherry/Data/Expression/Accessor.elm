module Cherry.Data.Expression.Accessor exposing
    ( Accessor(..)
    , decoder
    , parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))
import Pratt
import Set


-- TYPES -----------------------------------------------------------------------


{-| -}
type Accessor expression
    = Computed expression
    | Fixed String


-- PARSING JSON ----------------------------------------------------------------


decoder : Decoder expression -> Decoder (Accessor expression)
decoder expressionDecoder =
    Json.Decode.oneOf
        [ computedAccessorDecoder expressionDecoder
        , fixedAccessorDecoder
        ]

{-| -}
computedAccessorDecoder : Decoder expression -> Decoder (Accessor expression)
computedAccessorDecoder expressionDecoder =
    Json.Decode.Extra.taggedObject "Accessor.Computed" <|
        Json.Decode.map Computed
            (Json.Decode.field "key" expressionDecoder)

{-| -}
fixedAccessorDecoder : Decoder (Accessor expression)
fixedAccessorDecoder =
    Json.Decode.Extra.taggedObject "Accessor.Fixed" <|
        Json.Decode.map Fixed
            (Json.Decode.field "key" Json.Decode.string)


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : Pratt.Config expression -> Parser (Accessor expression)
parser prattConfig =
    Parser.oneOf
        [ computedAccessorParser prattConfig
        , fixedAccessorParser
        ]

{-| -}
computedAccessorParser : Pratt.Config expression -> Parser (Accessor expression)
computedAccessorParser prattConfig =
    Parser.succeed Computed
        |. Parser.symbol "["
        |. Parser.spaces 
        |= Pratt.subExpression 0 prattConfig
        |. Parser.spaces
        |. Parser.symbol "]"

{-| -}
fixedAccessorParser : Parser (Accessor expression)
fixedAccessorParser =
    Parser.succeed Fixed
        |. Parser.symbol "."
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
