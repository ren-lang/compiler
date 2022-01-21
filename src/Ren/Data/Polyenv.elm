module Ren.Data.Polyenv exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Data.Typing exposing (Typing)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Polyenv =
    Dict String Typing



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
empty : Polyenv
empty =
    Dict.empty


{-| -}
singleton : String -> Typing -> Polyenv
singleton =
    Dict.singleton



-- QUERIES ---------------------------------------------------------------------


{-| -}
dom : String -> Polyenv -> Bool
dom =
    Dict.member



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
insert : String -> Typing -> Polyenv -> Polyenv
insert =
    Dict.insert
