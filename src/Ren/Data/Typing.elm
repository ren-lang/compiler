module Ren.Data.Typing exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict
import Json.Decode
import Json.Encode
import Ren.Ast.Type as Type exposing (Type)
import Ren.Data.Monoenv as Monoenv exposing (Monoenv)
import Ren.Data.Subst exposing (Subst)
import Set exposing (Set)



-- TYPES -----------------------------------------------------------------------


type alias Typing =
    ( Monoenv, Type )



-- CONSTRUCTORS ----------------------------------------------------------------


from : Monoenv -> Type -> Typing
from env_ t =
    ( env_, t )


mono : String -> Type -> Typing
mono var t =
    ( Monoenv.singleton var t, t )


poly : Type -> Typing
poly t =
    ( Monoenv.empty, t )



-- QUERIES ---------------------------------------------------------------------


free : Typing -> Set String
free ( env_, t ) =
    Set.union (Type.free t) (Monoenv.free env_)


env : Typing -> Monoenv
env =
    Tuple.first


type_ : Typing -> Type
type_ =
    Tuple.second



-- MANIPULATIONS ---------------------------------------------------------------


reduce : String -> Typing -> Typing
reduce var (( _, t1 ) as typing) =
    let
        intersection =
            Type.free >> Set.intersect (Type.free t1)
    in
    Tuple.mapFirst (Monoenv.remove var >> Monoenv.filter (Basics.not << Set.isEmpty << intersection)) typing


remove : String -> Typing -> Typing
remove var typing =
    Tuple.mapFirst (Monoenv.remove var) typing


substitute : Subst Type -> Typing -> Typing
substitute s typing =
    Tuple.mapBoth (Monoenv.substitute s) (Type.substitute s) typing


updateEnv : (Monoenv -> Monoenv) -> Typing -> Typing
updateEnv =
    Tuple.mapFirst


updateType : (Type -> Type) -> Typing -> Typing
updateType =
    Tuple.mapSecond



-- CONVERSIONS -----------------------------------------------------------------


toString : Typing -> String
toString ( env_, t ) =
    String.join " " [ Monoenv.toString env_, "⊢", Type.toString t ]

toJson : Typing -> String
toJson =
    encode >> Json.Encode.encode 4

-- JSON ------------------------------------------------------------------------


encode : Typing -> Json.Encode.Value
encode =
    Debug.todo ""
