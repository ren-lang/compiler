module Ren.Samples.Syntax exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Parser
import Ren.Compiler as Ren



-- SOURCE ----------------------------------------------------------------------


source : String
source =
    """
import 'a/module' as A.Module exposing { some, functions }
import 'another/module' as Another.Module
import 'another/other/module' exposing { a, value }
import 'a/module/for/the/side/effects'

pub fun aPublicFunction = _ => ()
pub fun aPublicFunctionWithManyArguments = a b { c } [ d, { e } ] => {
    let f = 10
    ret ()
}

fun aPrivateFunction = _ => ()
fun aPrivateFunctionWithLocalBindings = _ => {
    let a = 10
    let b = 1 :: [ 2 ]

    ret when b is [ x, y ] => a + x + y
}
"""



-- OUTPUT ----------------------------------------------------------------------


ast : Result (List Parser.DeadEnd) Ren.Module
ast =
    Ren.parse source


output : String
output =
    Result.map (Ren.emit Ren.ESModule) ast
        |> Result.withDefault ""
