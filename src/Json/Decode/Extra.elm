module Json.Decode.Extra exposing (..)


-- IMPORTS ---------------------------------------------------------------------


import Json.Decode


-- 


{-| -}
taggedObject : String -> Json.Decode.Decoder a -> Json.Decode.Decoder a
taggedObject tag decoder =
    Json.Decode.field "$" Json.Decode.string
        |> Json.Decode.andThen
            (\t ->
                if t == tag then
                    decoder

                else
                    Json.Decode.fail <| "Mismatch when decoding tag. Expected '"
                        ++ tag ++ "' but got '" ++ t ++ "'."
            )