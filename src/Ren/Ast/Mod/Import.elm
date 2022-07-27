module Ren.Ast.Mod.Import exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Util.Json as Json
import Util.List as List



-- TYPES -----------------------------------------------------------------------


type alias Import =
    { path : String
    , source : Source
    , name : List String
    , unqualified : List String
    }


type Source
    = Local
    | Package
    | External



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


local : String -> List String -> List String -> Import
local path name unqualified =
    { path = path
    , source = Local
    , name = name
    , unqualified = unqualified
    }


package : String -> List String -> List String -> Import
package path name unqualified =
    { path = path
    , source = Package
    , name = name
    , unqualified = unqualified
    }


external : String -> List String -> List String -> Import
external path name unqualified =
    { path = path
    , source = External
    , name = name
    , unqualified = unqualified
    }



-- QUERIES ---------------------------------------------------------------------


isLocal : Import -> Bool
isLocal =
    .source >> (==) Local


isPackage : Import -> Bool
isPackage =
    .source >> (==) Package


isExternal : Import -> Bool
isExternal =
    .source >> (==) External


alike : Import -> Import -> Bool
alike a b =
    a.path == b.path && a.source == b.source && a.name == b.name



-- MANIPULATIONS ---------------------------------------------------------------


merge : Import -> Import -> Import
merge a b =
    if alike a b then
        { a | unqualified = List.uniques <| a.unqualified ++ b.unqualified }

    else
        a



-- CONVERSIONS -----------------------------------------------------------------
-- JSON ------------------------------------------------------------------------


encode : Import -> Json.Encode.Value
encode imp =
    let
        encodeSource source =
            case source of
                Local ->
                    Json.taggedEncoder "Local" [] []

                Package ->
                    Json.taggedEncoder "Package" [] []

                External ->
                    Json.taggedEncoder "External" [] []
    in
    Json.taggedEncoder "Import"
        []
        [ Json.Encode.string imp.path
        , encodeSource imp.source
        , Json.Encode.list Json.Encode.string imp.name
        , Json.Encode.list Json.Encode.string imp.unqualified
        ]


decoder : Json.Decode.Decoder Import
decoder =
    let
        sourceDecoder =
            Json.taggedDecoder
                (\str ->
                    case str of
                        "Local" ->
                            Json.Decode.succeed Local

                        "Package" ->
                            Json.Decode.succeed Package

                        "External" ->
                            Json.Decode.succeed External

                        _ ->
                            Json.Decode.fail <| "Unknown source: " ++ str
                )
    in
    Json.taggedDecoder
        (\key ->
            if key == "Import" then
                Json.Decode.map4 Import
                    (Json.Decode.index 1 <| Json.Decode.string)
                    (Json.Decode.index 2 <| sourceDecoder)
                    (Json.Decode.index 3 <| Json.Decode.list Json.Decode.string)
                    (Json.Decode.index 4 <| Json.Decode.list Json.Decode.string)

            else
                Json.Decode.fail <| "Unknown key: " ++ key
        )



-- UTILS -----------------------------------------------------------------------
