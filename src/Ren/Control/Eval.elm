module Ren.Control.Eval exposing (..)

-- TYPES -----------------------------------------------------------------------


type alias Eval ctx e a =
    ctx -> ( ctx, Result e a )



-- CONSTRUCTORS ----------------------------------------------------------------


succeed : a -> Eval ctx e a
succeed a =
    \ctx -> ( ctx, Ok a )


throw : e -> Eval ctx e a
throw e =
    \ctx -> ( ctx, Err e )



-- QUERIES ---------------------------------------------------------------------


context : Eval ctx e ctx
context =
    \ctx -> ( ctx, Ok ctx )



-- MANIPULATIONS ---------------------------------------------------------------


map : (a -> b) -> Eval ctx e a -> Eval ctx e b
map f step =
    andThen (f >> succeed) step


map2 : (a -> b -> c) -> Eval ctx e a -> Eval ctx e b -> Eval ctx e c
map2 f step1 step2 =
    andThen (\a -> map (f a) step2) step1


andThen : (a -> Eval ctx e b) -> Eval ctx e a -> Eval ctx e b
andThen f step =
    \ctx ->
        case step ctx of
            ( next, Ok a ) ->
                f a next

            ( next, Err e ) ->
                ( next, Err e )


andCatch : (e -> Eval ctx e a) -> Eval ctx e a -> Eval ctx e a
andCatch f step =
    \ctx ->
        case step ctx of
            ( _, Err e ) ->
                f e ctx

            ( next, Ok a ) ->
                ( next, Ok a )


bind : Eval ctx e a -> (a -> Eval ctx e b) -> Eval ctx e b
bind step f =
    andThen f step


andMap : Eval ctx e (a -> b) -> Eval ctx e a -> Eval ctx e b
andMap stepF stepA =
    map2 (<|) stepF stepA


traverse : (a -> Eval ctx e b) -> List a -> Eval ctx e (List b)
traverse f list =
    List.foldr (f >> map2 (::)) (succeed []) list


sequence : List (Eval ctx e a) -> Eval ctx e (List a)
sequence list =
    traverse Basics.identity list
