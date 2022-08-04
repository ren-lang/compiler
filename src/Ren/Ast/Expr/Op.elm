module Ren.Ast.Expr.Op exposing (..)

{-|


## Types

@docs Op


## Constants

@docs all, names, symbols


## Constructors

@docs fromName, fromSymbol


## Queries

@docs name, symbol


## JSON

@docs encode, decoder

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Util.List as List
import Util.Triple as Triple



-- TYPES -----------------------------------------------------------------------


type Op
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


all : List Op
all =
    List.map Triple.first allNamesAndSymbols


names : List String
names =
    List.map Triple.second allNamesAndSymbols


symbols : List String
symbols =
    List.map Triple.third allNamesAndSymbols


allNamesAndSymbols : List ( Op, String, String )
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


fromName : String -> Maybe Op
fromName n =
    List.indexOf n names
        |> Maybe.andThen (\i -> List.at i all)


fromSymbol : String -> Maybe Op
fromSymbol s =
    List.indexOf s symbols
        |> Maybe.andThen (\i -> List.at i all)



-- QUERIES ---------------------------------------------------------------------


name : Op -> String
name op =
    List.find (Triple.first >> (==) op) allNamesAndSymbols
        |> Maybe.map Triple.second
        -- This default should never be hit, we hardcode the list of operators
        -- and their names/symbols.
        |> Maybe.withDefault ""


symbol : Op -> String
symbol op =
    List.find (Triple.first >> (==) op) allNamesAndSymbols
        |> Maybe.map Triple.third
        -- This default should never be hit, we hardcode the list of operators
        -- and their names/symbols.
        |> Maybe.withDefault ""



-- JSON ------------------------------------------------------------------------


encode : Op -> Json.Encode.Value
encode op =
    Json.Encode.string <| symbol op


decoder : Json.Decode.Decoder Op
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case fromSymbol str of
                    Just op ->
                        Json.Decode.succeed op

                    Nothing ->
                        Json.Decode.fail <| "Unknown operator name: " ++ str
            )
