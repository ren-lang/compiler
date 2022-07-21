module Ren.Control.Parser exposing
    ( run
    , Parser(..), DeadEnd, Loop(..)
    , succeed, commit, problem
    , any, token
    , keyword, symbol, operator, identifier, end
    , number, string
    , map, map2, andThen
    , keep, drop
    , lazy, backtrackable
    , loop, many, oneOf
    , chompIf, chompWhile
    )

{-|

@docs run


## Types

@docs Parser, DeadEnd, Loop


## Constructors

@docs succeed, commit, problem
@docs any, token
@docs keyword, symbol, operator, identifier, end
@docs number, string


## Manipulations

@docs map, map2, andThen
@docs keep, drop


## Utils

@docs lazy, backtrackable
@docs loop, many, oneOf
@docs chompIf, chompWhile
@docs debug

-}

-- IMPORTS ---------------------------------------------------------------------

import Array exposing (Array)
import Ren.Ast.Expr as Expr
import Ren.Data.Token as Token exposing (Token)



--


{-| -}
run : Parser ctx e a -> List Token -> Result (List (DeadEnd ctx e)) a
run parser stream =
    let
        state =
            { stream = Array.fromList stream
            , offset = 0
            , context = []
            }
    in
    case runwrap parser state of
        Good _ value _ ->
            Ok value

        Bad _ bag ->
            Err (bagToList bag [])



-- TYPES -----------------------------------------------------------------------


{-| -}
type Parser ctx e a
    = Parser (State ctx -> Step ctx e a)


{-| ❓ The `Bool` in both variants dictates whether the parser should commit to
an error or not. When `False`, the parser is permitted to backtrack.
-}
type Step ctx e a
    = Good Bool a (State ctx)
    | Bad Bool (Bag ctx e)


{-| -}
type alias State ctx =
    { stream : Array Token
    , offset : Int
    , context : List ctx
    }


{-| -}
type alias DeadEnd ctx e =
    { offset : Int
    , error : e
    , contextStack : List ctx
    }


{-| -}
type Bag ctx e
    = Empty
    | AddRight (Bag ctx e) (DeadEnd ctx e)
    | Append (Bag ctx e) (Bag ctx e)


{-| -}
type Loop state a
    = Continue state
    | Break a



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
bagFromState : State ctx -> e -> Bag ctx e
bagFromState { offset, context } error =
    AddRight Empty <| DeadEnd offset error context


{-| -}
bagToList : Bag ctx e -> List (DeadEnd ctx e) -> List (DeadEnd ctx e)
bagToList bag errors =
    case bag of
        Empty ->
            errors

        AddRight left error ->
            bagToList left <| error :: errors

        Append left right ->
            bagToList left <| bagToList right errors



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
succeed : a -> Parser ctx e a
succeed a =
    Parser <| \state -> Good False a state


{-| -}
commit : a -> Parser ctx e a
commit a =
    Parser <| \state -> Good True a state


{-| -}
problem : e -> Parser ctx e a
problem error =
    Parser <| \state -> Bad False <| bagFromState state error



--


any : e -> Parser ctx e Token
any error =
    Parser <|
        \state ->
            case nextToken state of
                Token.EOF ->
                    Bad False <| bagFromState state error

                tok ->
                    Good True tok { state | offset = state.offset + 1 }


token : e -> Token -> Parser ctx e ()
token error tok =
    Parser <|
        \state ->
            if nextToken state == tok then
                Good True () { state | offset = state.offset + 1 }

            else
                Bad False <| bagFromState state error


{-| -}
keyword : e -> Token.Keyword -> Parser ctx e ()
keyword error kwd =
    token error <| Token.Keyword kwd


{-| -}
symbol : e -> Token.Symbol -> Parser ctx e ()
symbol error sym =
    token error <| Token.Symbol sym


{-| -}
operator : e -> Expr.Operator -> Parser ctx e ()
operator error op =
    token error <| Token.Operator op


{-| -}
end : e -> Parser ctx e ()
end error =
    Parser <|
        \state ->
            if Array.length state.stream <= state.offset then
                Good False () state

            else
                Bad False (bagFromState state error)



--


identifier : e -> Token.Case -> Parser ctx e String
identifier error casing =
    Parser <|
        \state ->
            case nextToken state of
                Token.Identifier c s ->
                    if c == casing then
                        Good True s { state | offset = state.offset + 1 }

                    else
                        Bad False <| bagFromState state error

                _ ->
                    Bad False <| bagFromState state error



--


{-| -}
number : e -> Parser ctx e Float
number expecting =
    Parser <|
        \state ->
            case nextToken state of
                Token.Number n ->
                    Good True n { state | offset = state.offset + 1 }

                _ ->
                    Bad False <| bagFromState state expecting


{-| -}
string : e -> Parser ctx e String
string expecting =
    Parser <|
        \state ->
            case nextToken state of
                Token.String s ->
                    Good True s { state | offset = state.offset + 1 }

                _ ->
                    Bad False <| bagFromState state expecting



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
map : (a -> b) -> Parser ctx e a -> Parser ctx e b
map f parser =
    Parser <|
        \state ->
            case runwrap parser state of
                Good c a nextState ->
                    Good c (f a) nextState

                Bad c error ->
                    Bad c error


{-| -}
map2 : (a -> b -> value) -> Parser ctx e a -> Parser ctx e b -> Parser ctx e value
map2 f parseA parseB =
    parseA |> andThen (\a -> map (f a) parseB)


{-| -}
andThen : (a -> Parser ctx e b) -> Parser ctx e a -> Parser ctx e b
andThen f parseA =
    Parser <|
        \state ->
            case runwrap parseA state of
                Bad c error ->
                    Bad c error

                Good c1 a nextState ->
                    case runwrap (f a) nextState of
                        Bad c2 error ->
                            Bad (c1 || c2) error

                        Good c2 b finalState ->
                            Good (c1 || c2) b finalState



--


{-| -}
keep : Parser ctx e a -> Parser ctx e (a -> b) -> Parser ctx e b
keep a f =
    map2 (<|) f a


{-| -}
drop : Parser ctx e ignore -> Parser ctx e keep -> Parser ctx e keep
drop b a =
    map2 Basics.always a b



-- UTILS -----------------------------------------------------------------------


{-| -}
lazy : (() -> Parser ctx e a) -> Parser ctx e a
lazy thunk =
    Parser <| \state -> runwrap (thunk ()) state


{-| -}
backtrackable : Parser ctx e a -> Parser ctx e a
backtrackable parser =
    Parser <|
        \state ->
            case runwrap parser state of
                Bad _ error ->
                    Bad False error

                Good _ val nextState ->
                    Good False val nextState



--


{-| -}
loop : state -> (state -> Parser ctx e (Loop state a)) -> Parser ctx e a
loop init f =
    let
        go p loopState state =
            case runwrap (f loopState) state of
                Good p1 (Continue nextLoopState) nextState ->
                    go (p || p1) nextLoopState nextState

                Good p1 (Break result) nextState ->
                    Good (p || p1) result nextState

                Bad p1 error ->
                    Bad (p || p1) error
    in
    Parser <| \state -> go False init state


{-| -}
many : (List a -> List (Parser ctx e (Loop (List a) (List a)))) -> Parser ctx e (List a)
many parsers =
    loop [] <| \xs -> oneOf <| parsers xs


{-| Just like [`Parser.oneOf`](Parser#oneOf)
-}
oneOf : List (Parser c x a) -> Parser c x a
oneOf parsers =
    Parser <| \s -> oneOfHelp s Empty parsers


oneOfHelp : State c -> Bag c x -> List (Parser c x a) -> Step c x a
oneOfHelp s0 bag parsers =
    case parsers of
        [] ->
            Bad False bag

        (Parser parse) :: remainingParsers ->
            case parse s0 of
                (Good _ _ _) as step ->
                    step

                (Bad p x) as step ->
                    if p then
                        step

                    else
                        oneOfHelp s0 (Append bag x) remainingParsers



--


{-| -}
chompIf : (Token -> Bool) -> e -> Parser ctx e ()
chompIf predicate expecting =
    Parser <|
        \state ->
            Array.get state.offset state.stream
                |> Maybe.map predicate
                |> Maybe.withDefault False
                |> (\matches ->
                        if matches then
                            Good True () { state | offset = state.offset + 1 }

                        else
                            Bad False (bagFromState state expecting)
                   )


{-| -}
chompWhile : (Token -> Bool) -> Parser ctx e ()
chompWhile predicate =
    let
        go offset state =
            Array.get offset state.stream
                |> Maybe.map predicate
                |> Maybe.withDefault False
                |> (\matches ->
                        if matches then
                            go (offset + 1) state

                        else
                            Good (state.offset < offset) () { state | offset = offset }
                   )
    in
    Parser <| \state -> go state.offset state



-- INTERNAL UTILS --------------------------------------------------------------


runwrap : Parser ctx e a -> State ctx -> Step ctx e a
runwrap (Parser parse) state =
    parse state


nextToken : State ctx -> Token
nextToken state =
    Array.get state.offset state.stream
        |> Maybe.withDefault Token.EOF
