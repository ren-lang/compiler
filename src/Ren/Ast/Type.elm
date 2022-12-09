module Ren.Ast.Type exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Control.Parser.Pratt as Pratt
import Ren.Data.Subst as Subst exposing (Subst)
import Ren.Data.Token as Token
import Set exposing (Set)
import Util.Json



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


type alias ParseContext =
    { inArgPosition : Bool
    }



-- CONSTRUCTORS ----------------------------------------------------------------


access : String -> Type
access k =
    fun [ rec [ ( k, var "a" ) ] ] (var "a")


arr : Type -> Type
arr a =
    App (Con "Arr") [ a ]


bool : Type
bool =
    sum [ ( "true", [] ), ( "false", [] ) ]


fun : List Type -> Type -> Type
fun args ret =
    List.foldr Fun ret args


num : Type
num =
    Con "Num"


rec : List ( String, Type ) -> Type
rec rows =
    Rec <| Dict.fromList <| List.map (Tuple.mapSecond List.singleton) rows


str : Type
str =
    Con "Str"


sum : List ( String, List Type ) -> Type
sum rows =
    Sum <| Dict.fromList rows


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
        case t of
            Any ->
                [ "*" ]

            App t1 tN ->
                toParenthesisedString t1 :: List.map toParenthesisedString tN

            Con c ->
                [ c ]

            Fun t1 t2 ->
                [ toParenthesisedString t1, "→", toString t2 ]

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


parser : ParseContext -> Parser () String Type
parser context =
    Pratt.expression
        { oneOf =
            List.concat
                [ List.map Pratt.literal
                    [ anyParser
                    , recParser
                    , varParser
                    ]
                , List.map (Parser.andThen (appParser context) >> Pratt.literal)
                    [ parenthesisedParser
                    , constructorParser
                    , holeParser
                    , sumParser context
                    ]
                ]
        , andThenOneOf =
            [ Pratt.infixRight 1 (Parser.symbol "" Token.Arrow) Fun
            ]
        , spaces = Parser.succeed ()
        }


parenthesisedParser : Parser () String Type
parenthesisedParser =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol "" <| Token.Paren Token.Left)
        |> Parser.keep (Parser.lazy <| \_ -> parser { inArgPosition = False })
        |> Parser.drop (Parser.symbol "" <| Token.Paren Token.Right)


anyParser : Parser () String Type
anyParser =
    Parser.succeed Any
        |> Parser.drop (Parser.symbol "" <| Token.Star)


appParser : ParseContext -> Type -> Parser () String Type
appParser { inArgPosition } con =
    if inArgPosition then
        Parser.succeed con

    else
        Parser.many (Parser.lazy <| \_ -> parser { inArgPosition = False })
            |> Parser.map
                (\args ->
                    if List.isEmpty args then
                        con

                    else
                        App con args
                )


constructorParser : Parser () String Type
constructorParser =
    Parser.succeed Con
        |> Parser.keep (Parser.identifier "" Token.Upper)


holeParser : Parser () String Type
holeParser =
    Parser.succeed Hole
        |> Parser.drop (Parser.symbol "" <| Token.Question)


recParser : Parser () String Type
recParser =
    let
        field =
            Parser.succeed (\k t -> ( k, [ t ] ))
                |> Parser.keep (Parser.identifier "" Token.Lower)
                |> Parser.drop (Parser.symbol "" <| Token.Colon)
                |> Parser.keep (Parser.lazy <| \_ -> parser { inArgPosition = False })

        fields =
            Parser.loop []
                (\fs ->
                    Parser.oneOf
                        [ Parser.succeed (\f -> f :: fs)
                            |> Parser.drop (Parser.symbol "" Token.Comma)
                            |> Parser.keep field
                            |> Parser.map Parser.Continue
                        , Parser.succeed (\_ -> List.reverse fs)
                            |> Parser.keep (Parser.symbol "" <| Token.Brace Token.Right)
                            |> Parser.map Parser.Break
                        ]
                )
    in
    Parser.succeed (Dict.fromList >> Rec)
        |> Parser.drop (Parser.symbol "" <| Token.Brace Token.Left)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.succeed (::)
                    |> Parser.keep field
                    |> Parser.keep fields
                , Parser.succeed []
                    |> Parser.drop (Parser.symbol "" <| Token.Brace Token.Right)
                ]
            )


sumParser : ParseContext -> Parser () String Type
sumParser { inArgPosition } =
    if inArgPosition then
        Parser.succeed (List.singleton >> Dict.fromList >> Sum)
            |> Parser.keep (variantParser { inArgPosition = False })

    else
        Parser.succeed (\x xs -> Sum <| Dict.fromList (x :: xs))
            |> Parser.keep (variantParser { inArgPosition = False })
            |> Parser.keep
                (Parser.many
                    (Parser.succeed Basics.identity
                        |> Parser.drop (Parser.symbol "" Token.Bar)
                        |> Parser.keep (variantParser { inArgPosition = False })
                    )
                )


variantParser : ParseContext -> Parser () String ( String, List Type )
variantParser { inArgPosition } =
    let
        args =
            Parser.many (Parser.lazy <| \_ -> parser { inArgPosition = False })
    in
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol "" <| Token.Colon)
        |> Parser.keep (Parser.identifier "" Token.Lower)
        |> Parser.andThen
            (\con ->
                if inArgPosition then
                    Parser.succeed ( con, [] )

                else
                    Parser.map (Tuple.pair con) args
            )


varParser : Parser () String Type
varParser =
    Parser.succeed Var
        |> Parser.keep (Parser.identifier "" Token.Lower)



-- JSON ------------------------------------------------------------------------


encode : Type -> Json.Encode.Value
encode t =
    case t of
        Any ->
            Util.Json.taggedEncoder "Any" [] []

        App t1 tN ->
            Util.Json.taggedEncoder "App" [] [ encode t1, Json.Encode.list encode tN ]

        Con c ->
            Util.Json.taggedEncoder "Con" [] [ Json.Encode.string c ]

        Fun t1 t2 ->
            Util.Json.taggedEncoder "Fun" [] [ encode t1, encode t2 ]

        Hole ->
            Util.Json.taggedEncoder "Hole" [] []

        Rec r ->
            Util.Json.taggedEncoder "Rec" [] [ Json.Encode.dict Basics.identity (Json.Encode.list encode) r ]

        Sum r ->
            Util.Json.taggedEncoder "Sum" [] [ Json.Encode.dict Basics.identity (Json.Encode.list encode) r ]

        Var v ->
            Util.Json.taggedEncoder "Var" [] [ Json.Encode.string v ]


decoder : Json.Decode.Decoder Type
decoder =
    let
        lazyDecoder =
            Json.Decode.lazy <| \_ -> decoder
    in
    Util.Json.taggedDecoder
        (\tag ->
            case tag of
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
                        (Json.Decode.index 1 <| Json.Decode.dict <| Json.Decode.list lazyDecoder)

                "Sum" ->
                    Json.Decode.map Sum
                        (Json.Decode.index 1 <| Json.Decode.dict <| Json.Decode.list lazyDecoder)

                "Var" ->
                    Json.Decode.map Var
                        (Json.Decode.index 1 <| Json.Decode.string)

                _ ->
                    Json.Decode.fail <| "Unknown tag: " ++ tag
        )
