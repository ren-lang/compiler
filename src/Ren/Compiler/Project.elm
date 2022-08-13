module Ren.Compiler.Project exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



-- TYPES -----------------------------------------------------------------------


type alias Project =
    {}



-- CONSTRUCTORS ----------------------------------------------------------------


init : Project
init =
    {}



-- CONVERSIONS -----------------------------------------------------------------


toJson : Project -> String
toJson proj =
    encode proj |> Json.Encode.encode 4



-- JSON ------------------------------------------------------------------------


encode : Project -> Json.Encode.Value
encode proj =
    Json.Encode.object
        []


decoder : Json.Decode.Decoder Project
decoder =
    Json.Decode.succeed Project
