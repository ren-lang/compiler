module Util.FFI exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



--


{-| -}
get : Json.Decode.Value -> String -> Json.Decode.Decoder a -> Maybe a
get proxy key expecting =
    let
        message =
            Json.Encode.object
                [ ( "method", Json.Encode.string key )
                ]
                |> Json.Encode.encode 0
    in
    Json.Decode.decodeValue (Json.Decode.field message expecting) proxy
        |> Result.toMaybe


{-| Call a function on an FFI proxy object by supplying the name of the function
to call and a list of JSON-encoded arguments.
-}
call : Json.Decode.Value -> String -> List Json.Encode.Value -> Json.Decode.Decoder a -> Maybe a
call proxy fn args expecting =
    let
        message =
            Json.Encode.object
                [ ( "method", Json.Encode.string fn )
                , ( "args", Json.Encode.list Basics.identity args )
                ]
                |> Json.Encode.encode 0
    in
    Json.Decode.decodeValue (Json.Decode.field message expecting) proxy
        |> Result.toMaybe
