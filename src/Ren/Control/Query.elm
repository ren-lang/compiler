module Ren.Control.Query exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Ren.Data.Error exposing (Error)
import Task exposing (Task)



-- TYPES -----------------------------------------------------------------------


type alias Query env a =
    Actions -> env -> Task ( env, Error ) ( env, a )


type alias Actions =
    { read : String -> Task Error String
    , emit : String -> Task Error ()
    , eval : String -> Task Error ()
    }



--


run : Actions -> env -> Query env a -> Cmd (Result ( env, String ) ( env, a ))
run actions env query =
    Task.attempt Basics.identity <| query actions env



-- ACTIONS AS QUERIES ----------------------------------------------------------


read : String -> Query env String
read path =
    \actions env ->
        actions.read path
            |> Task.map (Tuple.pair env)
            |> Task.mapError (Tuple.pair env)


emit : String -> Query env ()
emit src =
    \actions env ->
        actions.emit src
            |> Task.map (Tuple.pair env)
            |> Task.mapError (Tuple.pair env)


eval : String -> Query env ()
eval code =
    \actions env ->
        actions.eval code
            |> Task.map (Tuple.pair env)
            |> Task.mapError (Tuple.pair env)



-- CONSTRUCTORS ----------------------------------------------------------------


succeed : a -> Query env a
succeed a =
    \_ env ->
        Task.succeed ( env, a )


fail : String -> Query env a
fail msg =
    \_ env ->
        Task.fail ( env, msg )


fromResult : Result Error a -> Query env a
fromResult result =
    \_ env ->
        case result of
            Ok a ->
                Task.succeed ( env, a )

            Err e ->
                Task.fail ( env, e )


fromMaybe : Error -> Maybe a -> Query env a
fromMaybe error maybe =
    \_ env ->
        case maybe of
            Just a ->
                Task.succeed ( env, a )

            Nothing ->
                Task.fail ( env, error )


fromTask : Task Error a -> Query env a
fromTask task =
    \_ env ->
        task
            |> Task.map (Tuple.pair env)
            |> Task.mapError (Tuple.pair env)



-- COMBINATORS -----------------------------------------------------------------


andThen : (a -> Query env b) -> Query env a -> Query env b
andThen f query =
    \actions env ->
        query actions env
            |> Task.andThen (\( newEnv, a ) -> f a actions newEnv)


map : (a -> b) -> Query env a -> Query env b
map f query =
    \actions env ->
        query actions env
            |> Task.map (\( newStore, a ) -> ( newStore, f a ))


map2 : (a -> b -> c) -> Query env a -> Query env b -> Query env c
map2 f queryA queryB =
    do queryA <| \a ->
    do queryB <| \b ->
    succeed (f a b)


try : Query env a -> (Error -> Query env a) -> (a -> Query env b) -> Query env b
try query recover f =
    \actions env ->
        query actions env
            |> Task.onError (\( newEnv, err ) -> recover err actions newEnv)
            |> Task.andThen (\( newEnv, a ) -> f a actions newEnv)


nest : inner -> Query inner a -> (( inner, a ) -> Query outer b) -> Query outer b
nest inner query f =
    \actions outer ->
        (query actions inner
            |> Task.mapError Tuple.second
            |> fromTask
            |> andThen f
        )
            actions
            outer



--


traverse : (a -> Query env b) -> List a -> Query env (List b)
traverse f list =
    List.foldl (f >> map2 (::)) (succeed []) list
        |> map List.reverse


sequence : List (Query env a) -> Query env (List a)
sequence list =
    traverse Basics.identity list



-- MONADIC CHAINING ------------------------------------------------------------


do : Query env a -> (a -> Query env b) -> Query env b
do query f =
    andThen f query


doTask : Task Error a -> (a -> Query env b) -> Query env b
doTask task f =
    andThen f (fromTask task)



--


replace : b -> Query env a -> Query env b
replace b query =
    map (Basics.always b) query
