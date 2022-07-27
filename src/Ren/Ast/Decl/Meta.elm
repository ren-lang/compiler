module Ren.Ast.Decl.Meta exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Type as Type exposing (Type)



-- TYPES -----------------------------------------------------------------------


type alias Meta =
    { tipe : Type
    }



-- JSON ------------------------------------------------------------------------


encode : Meta -> List ( String, Json.Encode.Value )
encode meta =
    [ ( "type", Type.encode meta.tipe )
    ]


decoder : Json.Decode.Decoder Meta
decoder =
    Json.Decode.map Meta
        (Json.Decode.field "type" Type.decoder)
