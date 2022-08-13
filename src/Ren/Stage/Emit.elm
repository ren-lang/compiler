module Ren.Stage.Emit exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Ren.Ast.Decl as Decl exposing (Decl)
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Ast.Mod as Mod exposing (Mod)
import Ren.Stage.Emit.JavaScript as JavaScript



-- TYPES -----------------------------------------------------------------------


type Target
    = JavaScript
    | JSON
    | Types



--


mod : Target -> Mod -> String
mod target m =
    case target of
        JavaScript ->
            JavaScript.mod m

        JSON ->
            Mod.toJson m

        Types ->
            Debug.todo ""


decl : Target -> Decl -> String
decl target d =
    case target of
        JavaScript ->
            JavaScript.decl d

        JSON ->
            Decl.toJson d

        Types ->
            Debug.todo ""


expr : Target -> Expr -> String
expr target e =
    case target of
        JavaScript ->
            JavaScript.expr e

        JSON ->
            Expr.toJson e

        Types ->
            Debug.todo ""
