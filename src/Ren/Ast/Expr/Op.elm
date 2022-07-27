module Ren.Ast.Expr.Op exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Util.List as List
import Util.Triple as Triple



-- TYPES -----------------------------------------------------------------------


type Operator
    = Add --    +
    | And --    and
    | Concat -- ++
    | Cons --   ::
    | Div --    /
    | Eq --     =
    | Gte --    >=
    | Gt --     >
    | Lte --    <=
    | Lt --     <
    | Mod --    %
    | Mul --    *
    | Neq --    !=
    | Or --     or
    | Pipe --   |>
    | Sub --    -



-- CONSTANTS -------------------------------------------------------------------


all : List Operator
all =
    List.map Triple.first allNamesAndSymbols


names : List String
names =
    List.map Triple.second allNamesAndSymbols


symbols : List String
symbols =
    List.map Triple.third allNamesAndSymbols


allNamesAndSymbols : List ( Operator, String, String )
allNamesAndSymbols =
    [ ( Add, "add", "+" )
    , ( And, "and", "and" )
    , ( Concat, "concat", "++" )
    , ( Cons, "cons", "::" )
    , ( Div, "div", "/" )
    , ( Eq, "eq", "==" )
    , ( Gte, "gte", ">=" )
    , ( Gt, "gt", ">" )
    , ( Lte, "lte", "<=" )
    , ( Lt, "lt", "<" )
    , ( Mod, "mod", "%" )
    , ( Mul, "mul", "*" )
    , ( Neq, "neq", "!=" )
    , ( Or, "or", "or" )
    , ( Pipe, "pipe", "|>" )
    , ( Sub, "sub", "-" )
    ]



-- CONSTRUCTORS ----------------------------------------------------------------


fromName : String -> Maybe Operator
fromName n =
    List.indexOf n names
        |> Maybe.andThen (\i -> List.at i all)


fromSymbol : String -> Maybe Operator
fromSymbol s =
    List.indexOf s symbols
        |> Maybe.andThen (\i -> List.at i all)



-- QUERIES ---------------------------------------------------------------------


name : Operator -> String
name op =
    List.find (Triple.first >> (==) op) allNamesAndSymbols
        |> Maybe.map Triple.second
        -- This default should never be hit, we hardcode the list of operators
        -- and their names/symbols.
        |> Maybe.withDefault ""


symbol : Operator -> String
symbol op =
    List.find (Triple.first >> (==) op) allNamesAndSymbols
        |> Maybe.map Triple.third
        -- This default should never be hit, we hardcode the list of operators
        -- and their names/symbols.
        |> Maybe.withDefault ""



-- JSON ------------------------------------------------------------------------


encode : Operator -> Json.Encode.Value
encode op =
    Json.Encode.string <| name op


decoder : Json.Decode.Decoder Operator
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case fromName str of
                    Just op ->
                        Json.Decode.succeed op

                    Nothing ->
                        Json.Decode.fail <| "Unknown operator name: " ++ str
            )
