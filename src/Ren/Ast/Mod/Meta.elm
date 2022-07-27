module Ren.Ast.Mod.Meta exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



-- TYPES -----------------------------------------------------------------------


type alias Meta =
    { name : String
    , path : String
    , usesFFI : Bool
    }



-- JSON ------------------------------------------------------------------------


encode : Meta -> List ( String, Json.Encode.Value )
encode meta =
    [ ( "name", Json.Encode.string meta.name )
    , ( "path", Json.Encode.string meta.path )
    , ( "usesFFI", Json.Encode.bool meta.usesFFI )
    ]


decoder : Json.Decode.Decoder Meta
decoder =
    Json.Decode.map3 Meta
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "usesFFI" Json.Decode.bool)
