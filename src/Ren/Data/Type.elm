module Ren.Data.Type exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Data.Tuple2
import Dict exposing (Dict)
import Ren.Data.Substitution as Substitution
import Set exposing (Set)



-- TYPES -----------------------------------------------------------------------


{-| TODO: Our type system implemented in the Check module only deals with simple types.
We will need to expand this to work on other kinds of types, like rows (records
and enums), and potential even more kinds in the future.
-}
type Kind
    = Row Row
    | Type Type


{-| -}
type Row
    = Record Type (List ( String, Type ))
    | Variant Type (List ( String, Type ))


{-| -}
type Type
    = Var String
    | Con String
    | App Type (List Type)
    | Fun Type Type
    | Rec (Dict String Type)
    | Sum (Dict String (List Type))
    | Any
    | Hole


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


{-| Construct a function of any arity >= 1 by passing in the first parameter,
any other parameters, and the return type.
-}
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
var : Int -> String
var n =
    if n >= 26 then
        var ((n // 26) - 1) ++ var (modBy 26 n)

    else
        String.fromChar <| Char.fromCode <| 97 + modBy 26 n



-- QUERIES ---------------------------------------------------------------------


{-| This returns a set of all the free type variables in a type. As an example,
if we have the type `Either a (List b)` we would get back:

    Set.fromList [ "a", "b" ]

-}
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

        Rec tN ->
            Dict.foldl (always <| Set.union << free) Set.empty tN

        Sum tN ->
            Dict.foldl (\_ params set -> List.foldl (Set.union << free) set params) Set.empty tN

        Any ->
            Set.empty

        Hole ->
            Set.empty



-- MANIPULATIONS ---------------------------------------------------------------


{-| Applies a substitution to a type, effectively renaming all the type variables
in a type with those in the substitution map. This follows substitution chains so
if our map looks like:

    Dict.fromList
        [ ( "a", Var "b" )
        , ( "b", Con "String" )
        ]

and we apply the substitution to the type `List a` we will get back `List String`
like we'd expect.

-}
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

        Rec tN ->
            Rec <| Dict.map (always <| substitute s) tN

        Sum tN ->
            Sum <| Dict.map (always <| List.map (substitute s)) tN

        Any ->
            Any

        Hole ->
            Hole


{-| A specific type of substitution that reduces down all the type variables in
a type such that they start back from "a". That's kind of a weird explanation so
let's look at an example. Consider we have the the type:

    (d -> e) -> (e -> f) -> d -> f

(This is actually the type of the `(>>)` operator in both Ren and Elm!) By applying
`reduce` we get the easier-to-read:

    (a -> b) -> (b -> c) -> a -> c

-}
reduce : Type -> Type
reduce t =
    let
        -- Prevents substituting `a` for `a` and causing infinite loops.
        s i v =
            if var i == v then
                Substitution.empty

            else
                Substitution.singleton v (Var <| var i)
    in
    free t
        |> Set.toList
        |> List.indexedMap s
        |> List.foldl substitute t



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

        Rec tN ->
            "{ " ++ (String.join ", " <| List.map (Tuple.mapSecond toString >> Data.Tuple2.asList (String.join ": ")) <| Dict.toList tN) ++ " }"

        Sum tN ->
            String.join " | " <| List.map (Tuple.mapSecond (List.map toParenthesisedString >> String.join " ") >> Data.Tuple2.asList (String.join " ") >> (++) "#") <| Dict.toList tN

        Any ->
            "*"

        Hole ->
            "?"


toParenthesisedString : Type -> String
toParenthesisedString type_ =
    case type_ of
        App t1 [] ->
            toString t1

        App t1 t2 ->
            "(" ++ toString t1 ++ " " ++ (String.join " " <| List.map toParenthesisedString t2) ++ ")"

        Fun t1 t2 ->
            "(" ++ toParenthesisedString t1 ++ " → " ++ toString t2 ++ ")"

        Sum _ ->
            "(" ++ toString type_ ++ ")"

        _ ->
            toString type_
