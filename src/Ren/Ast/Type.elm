module Ren.Ast.Type exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Ren.Data.Subst as Subst exposing (Subst)
import Set exposing (Set)



-- TYPES -----------------------------------------------------------------------


type Type
    = Any --                    any type, e.g. "*"
    | App Type (List Type) --   type application, e.g. "Array a"
    | Con String --             concrete type constructor, e.g. "Number"
    | Fun Type Type --          function type, e.g. "Number -> Number"
    | Hole --                   unknown (to the user) type, e.g. "?"
    | Rec Row --                record type, e.g. "{x: Number, y: Number}"
    | Sum Row --                sum type, e.g. "#ok a | #err e"
    | Var String --             type variable, e.g. "a"


type alias Row =
    Dict String (List Type)



-- CONSTRUCTORS ----------------------------------------------------------------


access : String -> Type
access k =
    fun [ rec [ ( k, var "a" ) ] ] (var "a")


arr : Type -> Type
arr a =
    App (Con "Array") [ a ]


bool : Type
bool =
    sum [ ( "true", [] ), ( "false", [] ) ]


fun : List Type -> Type -> Type
fun args ret =
    List.foldr Fun ret args


num : Type
num =
    Con "Number"


rec : List ( String, Type ) -> Type
rec rows =
    Rec <| Dict.fromList <| List.map (Tuple.mapSecond List.singleton) rows


str : Type
str =
    Con "String"


sum : List ( String, List Type ) -> Type
sum rows =
    Sum <| Dict.fromList rows


undefined : Type
undefined =
    Sum <| Dict.singleton "undefined" []


var : String -> Type
var v =
    Var v


fresh : Int -> String
fresh n =
    if n >= 26 then
        fresh ((n // 26) - 1) ++ fresh (Basics.modBy 26 n)

    else
        String.fromChar <| Char.fromCode <| 97 + Basics.modBy 26 n



-- QUERIES ---------------------------------------------------------------------


free : Type -> Set String
free t =
    let
        freeInRow row =
            Dict.foldr (\_ tN s -> List.foldr (free >> Set.union) s tN) Set.empty row
    in
    case t of
        Any ->
            Set.empty

        App t1 tN ->
            List.foldr Set.union (free t1) (List.map free tN)

        Con _ ->
            Set.empty

        Fun t1 t2 ->
            Set.union (free t1) (free t2)

        Hole ->
            Set.empty

        Rec r ->
            freeInRow r

        Sum r ->
            freeInRow r

        Var v ->
            Set.singleton v


arg : Type -> Type
arg t =
    case t of
        Fun t1 _ ->
            t1

        _ ->
            t


return : Type -> Type
return t =
    case t of
        Fun _ t2 ->
            t2

        _ ->
            t


isAny : Type -> Bool
isAny t =
    case t of
        Any ->
            True

        _ ->
            False


isApp : Type -> Bool
isApp t =
    case t of
        App _ _ ->
            True

        _ ->
            False


isCon : Type -> Bool
isCon t =
    case t of
        Con _ ->
            True

        _ ->
            False


isFun : Type -> Bool
isFun t =
    case t of
        Fun _ _ ->
            True

        _ ->
            False


isHole : Type -> Bool
isHole t =
    case t of
        Hole ->
            True

        _ ->
            False


isRec : Type -> Bool
isRec t =
    case t of
        Rec _ ->
            True

        _ ->
            False


isSum : Type -> Bool
isSum t =
    case t of
        Sum _ ->
            True

        _ ->
            False


isVar : Type -> Bool
isVar t =
    case t of
        Var _ ->
            True

        _ ->
            False



-- MANIPULATIONS ---------------------------------------------------------------


substitute : Subst Type -> Type -> Type
substitute s t =
    let
        substituteRow row =
            Dict.map (\_ tR -> List.map (substitute s) tR) row
    in
    case t of
        Any ->
            Any

        App t1 tN ->
            App (substitute s t1) (List.map (substitute s) tN)

        Con c ->
            Con c

        Fun t1 t2 ->
            Fun (substitute s t1) (substitute s t2)

        Hole ->
            Hole

        Rec r ->
            Rec (substituteRow r)

        Sum r ->
            Sum (substituteRow r)

        Var v ->
            Subst.lookup v s
                -- Recursively apply the substitution to follow any chains of
                -- type variables.
                |> Maybe.map (substitute s)
                |> Maybe.withDefault t


simplify : Type -> Type
simplify t =
    let
        subst idx v =
            if fresh idx == v then
                Subst.empty

            else
                Subst.singleton v (Var <| fresh idx)
    in
    free t
        |> Set.toList
        |> List.indexedMap subst
        |> List.foldl substitute t



-- CONVERSIONS -----------------------------------------------------------------


toString : Type -> String
toString t =
    let
        toFields r =
            Dict.toList r
                |> List.map (\( k, tN ) -> String.join " " <| k :: ":" :: List.map toString tN)
                |> List.intersperse ","

        toVariants r =
            Dict.toList r
                |> List.map (\( k, tN ) -> String.join " " <| ("#" ++ k) :: List.map toParenthesisedString tN)
                |> List.intersperse "|"
    in
    String.join " " <|
        case simplify t of
            Any ->
                [ "*" ]

            App t1 tN ->
                toParenthesisedString t1 :: List.map toParenthesisedString tN

            Con c ->
                [ c ]

            Fun t1 t2 ->
                [ toParenthesisedString t1, "->", toString t2 ]

            Hole ->
                [ "?" ]

            Rec r ->
                if Dict.isEmpty r then
                    [ "{}" ]

                else
                    "{" :: toFields r ++ [ "}" ]

            Sum r ->
                if Dict.isEmpty r then
                    [ "[]" ]

                else
                    "[" :: toVariants r ++ [ "]" ]

            Var v ->
                [ v ]


toParenthesisedString : Type -> String
toParenthesisedString t =
    case t of
        App _ _ ->
            "(" ++ toString t ++ ")"

        Fun _ _ ->
            "(" ++ toString t ++ ")"

        _ ->
            toString t


toJson : Type -> String
toJson =
    encode >> Json.Encode.encode 4



-- PARSING ---------------------------------------------------------------------
-- JSON ------------------------------------------------------------------------


encode : Type -> Json.Encode.Value
encode t =
    Json.Encode.list Basics.identity <|
        case t of
            Any ->
                [ Json.Encode.object [ ( "$", Json.Encode.string "Any" ) ] ]

            App t1 tN ->
                [ Json.Encode.object [ ( "$", Json.Encode.string "App" ) ]
                , encode t1
                , Json.Encode.list encode tN
                ]

            Con c ->
                [ Json.Encode.object [ ( "$", Json.Encode.string "Con" ) ]
                , Json.Encode.string c
                ]

            Fun t1 t2 ->
                [ Json.Encode.object [ ( "$", Json.Encode.string "Fun" ) ]
                , encode t1
                , encode t2
                ]

            Hole ->
                [ Json.Encode.object [ ( "$", Json.Encode.string "Hole" ) ] ]

            Rec r ->
                [ Json.Encode.object [ ( "$", Json.Encode.string "Rec" ) ]
                , encodeRow r
                ]

            Sum r ->
                [ Json.Encode.object [ ( "$", Json.Encode.string "Sum" ) ]
                , encodeRow r
                ]

            Var v ->
                [ Json.Encode.object [ ( "$", Json.Encode.string "Var" ) ]
                , Json.Encode.string v
                ]


encodeRow : Row -> Json.Encode.Value
encodeRow row =
    Json.Encode.list Basics.identity
        [ Json.Encode.object [ ( "$", Json.Encode.string "Row" ) ]
        , Json.Encode.dict Basics.identity (Json.Encode.list encode) row
        ]


decoder : Json.Decode.Decoder Type
decoder =
    let
        lazyDecoder =
            Json.Decode.lazy <| \_ -> decoder
    in
    Json.Decode.index 0 (Json.Decode.field "$" Json.Decode.string)
        |> Json.Decode.andThen
            (\key ->
                case key of
                    "Any" ->
                        Json.Decode.succeed Any

                    "App" ->
                        Json.Decode.map2 App
                            (Json.Decode.index 1 <| lazyDecoder)
                            (Json.Decode.index 2 <| Json.Decode.list lazyDecoder)

                    "Con" ->
                        Json.Decode.map Con
                            (Json.Decode.index 1 <| Json.Decode.string)

                    "Fun" ->
                        Json.Decode.map2 Fun
                            (Json.Decode.index 1 <| lazyDecoder)
                            (Json.Decode.index 2 <| lazyDecoder)

                    "Hole" ->
                        Json.Decode.succeed Hole

                    "Rec" ->
                        Json.Decode.map Rec
                            (Json.Decode.index 1 <| rowDecoder)

                    "Sum" ->
                        Json.Decode.map Sum
                            (Json.Decode.index 1 <| rowDecoder)

                    "Var" ->
                        Json.Decode.map Var
                            (Json.Decode.index 1 <| Json.Decode.string)

                    _ ->
                        Json.Decode.fail <| "Unknown type: " ++ key
            )


rowDecoder : Json.Decode.Decoder Row
rowDecoder =
    Json.Decode.index 0 (Json.Decode.field "$" Json.Decode.string)
        |> Json.Decode.andThen
            (\key ->
                if key == "Row" then
                    Json.Decode.index 1 <| Json.Decode.dict (Json.Decode.list decoder)

                else
                    Json.Decode.fail <| "Unknown type: " ++ key
            )
