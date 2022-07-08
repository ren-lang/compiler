module Ren.Data.Declaration exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Ast.Expr as Expr exposing (Expr)



-- TYPES -----------------------------------------------------------------------


type Declaration
    = Let Bool String Expr
    | Ext Bool String String



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


local : Bool -> String -> Expr -> Declaration
local =
    Let


external : Bool -> String -> String -> Declaration
external =
    Ext



-- QUERIES ---------------------------------------------------------------------


name : Declaration -> String
name dec =
    case dec of
        Let _ name_ _ ->
            name_

        Ext _ name_ _ ->
            name_


isPublic : Declaration -> Bool
isPublic dec =
    case dec of
        Let pub _ _ ->
            pub

        Ext pub _ _ ->
            pub


isLocal : Declaration -> Bool
isLocal dec =
    case dec of
        Let _ _ _ ->
            True

        Ext _ _ _ ->
            False


isExternal : Declaration -> Bool
isExternal dec =
    case dec of
        Let _ _ _ ->
            False

        Ext _ _ _ ->
            True



-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------
