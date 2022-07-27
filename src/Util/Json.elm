module Util.Json exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



-- DECODERS --------------------------------------------------------------------


taggedDecoder : (String -> Json.Decode.Decoder a) -> Json.Decode.Decoder a
taggedDecoder decoder =
    Json.Decode.index 0 (Json.Decode.field "$" Json.Decode.string)
        |> Json.Decode.andThen decoder



-- ENCODERS --------------------------------------------------------------------


taggedEncoder : String -> List ( String, Json.Encode.Value ) -> List Json.Encode.Value -> Json.Encode.Value
taggedEncoder tag meta data =
    Json.Encode.list Basics.identity
        (Json.Encode.object (( "$", Json.Encode.string tag ) :: meta) :: data)
