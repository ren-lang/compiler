module Ren.Ast.Mod.Meta exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



-- TYPES -----------------------------------------------------------------------


type alias Meta =
    { name : String
    , path : String
    , pkgPath : String
    , usesFFI : Bool
    }



-- MANIPULATIONS ---------------------------------------------------------------


setName : String -> Meta -> Meta
setName name meta =
    { meta | name = name }


setPath : String -> Meta -> Meta
setPath path meta =
    { meta | path = path }


setPkgPath : String -> Meta -> Meta
setPkgPath pkgPath meta =
    { meta | pkgPath = pkgPath }


usesFFI : Bool -> Meta -> Meta
usesFFI b meta =
    { meta | usesFFI = b }



-- JSON ------------------------------------------------------------------------


encode : Meta -> List ( String, Json.Encode.Value )
encode meta =
    [ ( "name", Json.Encode.string meta.name )
    , ( "path", Json.Encode.string meta.path )
    , ( "pkgPath", Json.Encode.string meta.pkgPath )
    , ( "usesFFI", Json.Encode.bool meta.usesFFI )
    ]


decoder : Json.Decode.Decoder Meta
decoder =
    Json.Decode.map4 Meta
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "pkgPath" Json.Decode.string)
        (Json.Decode.field "usesFFI" Json.Decode.bool)
