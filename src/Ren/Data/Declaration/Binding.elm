module Ren.Data.Declaration.Binding exposing 
    ( Binding(..)
    , decoder
    , parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))
import Parser.Extra
import Ren.Data.Expression as Expression exposing (Expression)
import Ren.Data.Expression.Pattern as Pattern


-- TYPES -----------------------------------------------------------------------


{-| -}
type Binding
    = Binding Expression.Pattern Expression


-- PARSING JSON ----------------------------------------------------------------


{-| -}
decoder : Decoder Binding
decoder =
    Json.Decode.Extra.taggedObject "Binding" <|
        Json.Decode.map2 Binding
            (Json.Decode.field "name" Pattern.decoder)
            (Json.Decode.field "body" Expression.decoder)


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : Parser Binding
parser =
    Parser.succeed Binding
        |= Pattern.parser
        |. Parser.Extra.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Expression.parser
