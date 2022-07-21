module Ren.Data.Metadata exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Metadata =
    {}



-- JSON ------------------------------------------------------------------------


decoder : Json.Decode.Decoder ( String, Metadata )
decoder =
    Json.Decode.index 0 (Json.Decode.field "$" Json.Decode.string)
        |> Json.Decode.andThen
            (\key ->
                Json.Decode.succeed ( key, {} )
            )


encode : String -> Metadata -> Json.Encode.Value
encode key _ =
    Json.Encode.object
        [ ( "$", Json.Encode.string key )
        ]
