module Ren.Data.Typing exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict
import Json.Decode
import Json.Encode
import Ren.Ast.Type as Type exposing (Type)
import Ren.Data.Monoenv as Monoenv exposing (Monoenv)
import Ren.Data.Subst exposing (Subst)
import Set exposing (Set)
import Util.Json



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
encode ( env_, t ) =
    Util.Json.taggedEncoder "Typing"
        []
        [ Monoenv.encode env_
        , Type.encode t
        ]


decoder : Json.Decode.Decoder Typing
decoder =
    Util.Json.taggedDecoder
        (\tag ->
            if tag == "Typing" then
                Json.Decode.map2 Tuple.pair
                    (Json.Decode.index 1 <| Monoenv.decoder)
                    (Json.Decode.index 2 <| Type.decoder)

            else
                Json.Decode.fail <| "Unexpected tag: " ++ tag
        )
