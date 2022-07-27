module Ren.Data.Polyenv exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Data.Typing exposing (Typing)



-- TYPES -----------------------------------------------------------------------


type alias Polyenv =
    Dict String Typing



-- CONSTRUCTORS ----------------------------------------------------------------


empty : Polyenv
empty =
    Dict.empty


singleton : String -> Typing -> Polyenv
singleton var typing =
    Dict.singleton var typing



-- QUERIES ---------------------------------------------------------------------


contains : String -> Polyenv -> Bool
contains var env =
    Dict.member var env


lookup : String -> Polyenv -> Maybe Typing
lookup var env =
    Dict.get var env



-- MANIPULATIONS ---------------------------------------------------------------


insert : String -> Typing -> Polyenv -> Polyenv
insert var typing env =
    Dict.insert var typing env
