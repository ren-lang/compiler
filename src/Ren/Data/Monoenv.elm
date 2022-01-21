module Ren.Data.Monoenv exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Data.Tuple2
import Dict exposing (Dict)
import Ren.Data.Type as Type exposing (Type)
import Set exposing (Set)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Monoenv =
    Dict String Type



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
empty : Monoenv
empty =
    Dict.empty


{-| -}
singleton : String -> Type -> Monoenv
singleton =
    Dict.singleton



-- QUERIES ---------------------------------------------------------------------


{-| -}
all : Monoenv -> List ( String, Type )
all =
    Dict.toList


{-| -}
get : String -> Monoenv -> Maybe Type
get =
    Dict.get


{-| -}
free : Monoenv -> Set String
free env =
    Dict.values env
        |> List.foldl (Set.union << Type.free) Set.empty


{-| -}
names : Monoenv -> List String
names =
    Dict.keys


dom : String -> Monoenv -> Bool
dom =
    Dict.member



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
insert : String -> Type -> Monoenv -> Monoenv
insert =
    Dict.insert


{-| -}
substitute : Type.Substitution -> Monoenv -> Monoenv
substitute s env =
    Dict.map (always <| Type.substitute s) env


{-| -}
merge : Type.Substitution -> Monoenv -> Monoenv -> Monoenv
merge s a b =
    Dict.union (substitute s a) (substitute s b)


{-| -}
remove : String -> Monoenv -> Monoenv
remove =
    Dict.remove



-- CONVERSIONS -----------------------------------------------------------------


toString : Monoenv -> String
toString env =
    if Dict.isEmpty env then
        "{}"

    else
        "{ " ++ (String.join ", " <| List.map (Data.Tuple2.asList (String.join " :: ") << Tuple.mapSecond Type.toString) <| Dict.toList env) ++ " }"
