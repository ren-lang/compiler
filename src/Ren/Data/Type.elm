module Ren.Data.Type exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Data.Substitution as Substitution
import Set exposing (Set)



-- TYPES -----------------------------------------------------------------------


{-| -}
type Type
    = Var String
    | Con String
    | App Type (List Type)
    | Fun Type Type
    | Record (Dict String Type)
    | Enum Bool (List ( String, List Type ))
    | Any


{-| -}
type alias Error =
    String


{-| -}
type alias Substitution =
    Substitution.Substitution Type



-- CONSTRUCTORS ----------------------------------------------------------------


array : Type -> Type
array t =
    App (Con "Array") [ t ]


boolean : Type
boolean =
    Con "Boolean"


fun : Type -> List Type -> Type -> Type
fun f args ret =
    List.foldr Fun ret args
        |> Fun f


number : Type
number =
    Con "Number"


string : Type
string =
    Con "String"


{-| Generates type variables like "a", "b", ..., "z", "aa", "ab", ...
-}
var n =
    if n >= 26 then
        var ((n // 26) - 1) ++ var (modBy 26 n)

    else
        String.fromChar <| Char.fromCode <| 97 + modBy 26 n



-- QUERIES ---------------------------------------------------------------------


free : Type -> Set String
free type_ =
    case type_ of
        Var v ->
            Set.singleton v

        Con _ ->
            Set.empty

        App t1 tN ->
            List.foldl (Set.union << free) (free t1) tN

        Fun t1 t2 ->
            Set.union (free t1) (free t2)

        Record fields ->
            List.foldl (Set.union << free) Set.empty <| Dict.values fields

        Enum _ variants ->
            List.foldl (Set.union << List.foldl (Set.union << free) Set.empty << Tuple.second) Set.empty variants

        Any ->
            Set.empty



-- MANIPULATIONS ---------------------------------------------------------------


substitute : Substitution -> Type -> Type
substitute s t =
    case t of
        Var v ->
            Dict.get v s
                |> Maybe.map (substitute s)
                |> Maybe.withDefault (Var v)

        Con c ->
            Con c

        App t1 tN ->
            App (substitute s t1)
                (List.map (substitute s) tN)

        Fun t1 t2 ->
            Fun (substitute s t1) (substitute s t2)

        Record fields ->
            Record (Dict.map (always <| substitute s) fields)

        Enum open variants ->
            Enum open (List.map (Tuple.mapSecond <| List.map (substitute s)) variants)

        Any ->
            Any



-- CONVERSIONS -----------------------------------------------------------------


toString : Type -> String
toString type_ =
    case type_ of
        Var v ->
            v

        Con c ->
            c

        App t1 t2 ->
            toString t1 ++ " " ++ (String.join " " <| List.map toParenthesisedString t2)

        Fun ((Fun _ _) as t1) t2 ->
            toParenthesisedString t1 ++ " → " ++ toString t2

        Fun t1 t2 ->
            toString t1 ++ " → " ++ toString t2

        Record _ ->
            Debug.todo ""

        Enum _ _ ->
            Debug.todo ""

        Any ->
            "*"


toParenthesisedString : Type -> String
toParenthesisedString type_ =
    case type_ of
        App t1 [] ->
            toString t1

        App t1 t2 ->
            "(" ++ toString t1 ++ " " ++ (String.join " " <| List.map toParenthesisedString t2) ++ ")"

        Fun t1 t2 ->
            "(" ++ toParenthesisedString t1 ++ " → " ++ toString t2 ++ ")"

        Record fields ->
            Debug.todo ""

        Enum _ variants ->
            Debug.todo ""

        _ ->
            toString type_
