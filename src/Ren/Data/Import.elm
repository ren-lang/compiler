module Ren.Data.Import exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Encode
import Ren.Data.Metadata as Metadata
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
            Json.Encode.list Basics.identity <|
                case source of
                    Local ->
                        [ Metadata.encode "Local" {} ]

                    Package ->
                        [ Metadata.encode "Package" {} ]

                    External ->
                        [ Metadata.encode "External" {} ]
    in
    Json.Encode.list Basics.identity
        [ Metadata.encode "Import" {}
        , Json.Encode.string imp.path
        , encodeSource imp.source
        , Json.Encode.list Json.Encode.string imp.name
        , Json.Encode.list Json.Encode.string imp.unqualified
        ]



-- UTILS -----------------------------------------------------------------------
