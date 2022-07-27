module Ren.Control.Eval.Context exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Ren.Control.Eval exposing (Eval)



-- QUERIES ---------------------------------------------------------------------


get : Eval ctx e ctx
get =
    \ctx -> ( ctx, Ok ctx )



-- MANIPULATIONS ---------------------------------------------------------------


set : ctx -> Eval ctx e ()
set ctx =
    \_ -> ( ctx, Ok () )


update : (ctx -> ctx) -> Eval ctx e ()
update f =
    \ctx -> ( f ctx, Ok () )
