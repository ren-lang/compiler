module Ren.Data.Typing exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Data.Tuple2
import Ren.Data.Monoenv as Monoenv exposing (Monoenv)
import Ren.Data.Substitution as Substitution
import Ren.Data.Type as Type exposing (Type)
import Set exposing (Set)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Typing =
    ( Monoenv, Type )



-- CONSTRUCTORS ----------------------------------------------------------------


mono : String -> Type -> Typing
mono v t =
    ( Monoenv.singleton v t, t )


poly : Type -> Typing
poly t =
    ( Monoenv.empty, t )


from : Monoenv -> Type -> Typing
from =
    Tuple.pair



-- QUERIES ---------------------------------------------------------------------


free : Typing -> Set String
free ( env_, t ) =
    Monoenv.free env_
        |> Set.union (Type.free t)


type_ : Typing -> Type
type_ =
    Tuple.second


env : Typing -> Monoenv
env =
    Tuple.first



-- MANIPULATIONS ---------------------------------------------------------------


substitute : Type.Substitution -> Typing -> Typing
substitute s ( env_, t ) =
    ( Monoenv.substitute s env_, Type.substitute s t )


remove : String -> Typing -> Typing
remove =
    Monoenv.remove >> Tuple.mapFirst


reduce : Typing -> Typing
reduce typing =
    let
        -- Prevents substituting `a` for `a` and causing infinite loops.
        s i v =
            if Type.var i == v then
                Substitution.empty

            else
                Substitution.singleton v (Type.Var <| Type.var i)
    in
    free typing
        |> Set.toList
        |> List.indexedMap s
        |> List.foldl substitute typing



-- CONVERSIONS -----------------------------------------------------------------


{-| -}
toString : Typing -> String
toString typing =
    reduce typing
        |> Tuple.mapBoth Monoenv.toString Type.toString
        |> Data.Tuple2.asList (String.join " ⊢ ")


{-| -}
toForallString : Typing -> String
toForallString typing =
    reduce typing
        |> (\( env_, t ) ->
                if Set.isEmpty (free ( env_, t )) then
                    Type.toString t

                else
                    "∀ " ++ (String.join " " <| Set.toList <| free <| reduce typing) ++ ". " ++ Type.toString t
           )
