module Json.Encode.Extra exposing (..)


-- IMPORTS ---------------------------------------------------------------------


import Json.Encode


-- ENCODING OBJECTS ------------------------------------------------------------


taggedObject : String -> List ( String, Json.Encode.Value ) -> Json.Encode.Value
taggedObject tag fields =
    Json.Encode.object <|
        ( "$", Json.Encode.string tag ) :: fields
