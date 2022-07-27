module Ren.Data.Subst exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)



-- TYPES -----------------------------------------------------------------------


type alias Subst t =
    Dict String t



-- CONSTRUCTORS ----------------------------------------------------------------


empty : Subst t
empty =
    Dict.empty


singleton : String -> t -> Subst t
singleton var t =
    Dict.singleton var t



-- QUERIES ---------------------------------------------------------------------


contains : String -> Subst t -> Bool
contains var s =
    Dict.member var s


lookup : String -> Subst t -> Maybe t
lookup var s =
    Dict.get var s



-- MANIPULATIONS ---------------------------------------------------------------


insert : String -> t -> Subst t -> Subst t
insert var t subst =
    Dict.insert var t subst


compose : (Subst t -> t -> t) -> Subst t -> Subst t -> Subst t
compose apply s1 s2 =
    Dict.union s1 <| Dict.map (\_ t -> apply s1 t) s2
