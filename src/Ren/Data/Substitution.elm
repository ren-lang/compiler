module Ren.Data.Substitution exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Substitution a =
    Dict String a



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
empty : Substitution a
empty =
    Dict.empty


{-| -}
singleton : String -> a -> Substitution a
singleton =
    Dict.singleton



-- QUERIES ---------------------------------------------------------------------


contains : String -> Substitution a -> Bool
contains =
    Dict.member



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
insert : String -> a -> Substitution a -> Substitution a
insert =
    Dict.insert


{-| -}
compose : (Substitution a -> a -> a) -> Substitution a -> Substitution a -> Substitution a
compose apply s1 s2 =
    Dict.union s1 (Dict.map (always <| apply s1) s2)
