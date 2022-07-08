module Ren.Data.Import exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

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
-- UTILS -----------------------------------------------------------------------
