module Parser.Expression.Identifier exposing 
    ( suite
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Parse.Expression as Expression
import Cherry.Stage.Parse.Expression.Literal as Identifier
import Parser

import Expect
import Fuzz exposing (..)
import Test exposing (..)


-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "AST.Identifier"
        [
        ]
