module Ren.Data.Declaration.Visibility exposing 
    ( Visibility(..)
    , decoder
    , parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))


-- TYPES -----------------------------------------------------------------------


{-| -}
type Visibility
    = Public
    | Private


-- PARSING JSON ----------------------------------------------------------------


{-| -}
decoder : Decoder Visibility
decoder =
    Json.Decode.oneOf
        [ Json.Decode.Extra.taggedObject "Visibility.Public" <|
            Json.Decode.succeed Public
        , Json.Decode.Extra.taggedObject "Visibility.Private" <|
            Json.Decode.succeed Private
        ]


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : Parser Visibility
parser =
    Parser.oneOf
        [ Parser.succeed Public
            |. Parser.keyword "pub"
        , Parser.succeed Private
        ]