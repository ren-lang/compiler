module Ren.Control.Query.Env exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Ren.Control.Query as Query exposing (Query)
import Ren.Data.Error exposing (Error)
import Task



-- QUERIES ---------------------------------------------------------------------


get : Query env env
get =
    \_ env ->
        Task.succeed ( env, env )



-- MANIPULATIONS ---------------------------------------------------------------


set : env -> Query env env
set env =
    \_ _ ->
        Task.succeed ( env, env )


update : (env -> env) -> Query env ()
update f =
    \_ env ->
        Task.succeed ( f env, () )


map : (old -> new) -> (new -> old) -> Query old a -> Query new a
map f g query =
    \actions env ->
        query actions (g env)
            |> Task.map (Tuple.mapFirst f)
            |> Task.mapError (Tuple.mapFirst f)


mapPartial : (old -> new) -> (new -> Result Error old) -> Query old a -> Query new a
mapPartial f g query =
    \actions env ->
        case g env of
            Ok env_ ->
                query actions env_
                    |> Task.map (Tuple.mapFirst f)
                    |> Task.mapError (Tuple.mapFirst f)

            Err err ->
                Task.fail ( env, err )
