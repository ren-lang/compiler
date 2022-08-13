module Ren.Data.Monoenv exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Ren.Ast.Type as Type exposing (Type)
import Ren.Data.Subst exposing (Subst)
import Set exposing (Set)
import Util.Json



-- TYPES -----------------------------------------------------------------------


type alias Monoenv =
    Dict String Type



-- CONSTRUCTORS ----------------------------------------------------------------


empty : Monoenv
empty =
    Dict.empty


singleton : String -> Type -> Monoenv
singleton var t =
    Dict.singleton var t



-- QUERIES ---------------------------------------------------------------------


all : Monoenv -> List ( String, Type )
all env =
    Dict.toList env


contains : String -> Monoenv -> Bool
contains var env =
    Dict.member var env


free : Monoenv -> Set String
free env =
    Dict.foldl (\_ t f -> Set.union f <| Type.free t) Set.empty env


lookup : String -> Monoenv -> Maybe Type
lookup var env =
    Dict.get var env



-- MANIPULATIONS ---------------------------------------------------------------


filter : (Type -> Bool) -> Monoenv -> Monoenv
filter p env =
    Dict.filter (\_ t -> p t) env


insert : String -> Type -> Monoenv -> Monoenv
insert var t env =
    Dict.insert var t env


merge : Subst Type -> Monoenv -> Monoenv -> Monoenv
merge s env1 env2 =
    Dict.union (substitute s env1) (substitute s env2)


remove : String -> Monoenv -> Monoenv
remove var env =
    Dict.remove var env


substitute : Subst Type -> Monoenv -> Monoenv
substitute s env =
    Dict.map (\_ t -> Type.substitute s t) env



-- CONVERSIONS -----------------------------------------------------------------


toList : Monoenv -> List ( String, Type )
toList env =
    Dict.toList env


toString : Monoenv -> String
toString env =
    if Dict.isEmpty env then
        "{}"

    else
        String.join " " <|
            [ "{"
            , Dict.toList env
                |> List.map (\( v, t ) -> String.join " " [ v, ":", Type.toString t ])
                |> String.join ", "
            , "}"
            ]



-- JSON ------------------------------------------------------------------------


encode : Monoenv -> Json.Encode.Value
encode env =
    Util.Json.taggedEncoder "Monoenv"
        []
        [ Json.Encode.dict Basics.identity Type.encode env ]


decoder : Json.Decode.Decoder Monoenv
decoder =
    Util.Json.taggedDecoder
        (\tag ->
            if tag == "Monoenv" then
                Json.Decode.index 1 <| Json.Decode.dict Type.decoder

            else
                Json.Decode.fail <| "expected Monoenv, got " ++ tag
        )
