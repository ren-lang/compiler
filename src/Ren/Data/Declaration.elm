module Ren.Data.Declaration exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Data.Metadata as Metadata



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
-- JSON ------------------------------------------------------------------------


encode : Declaration -> Json.Encode.Value
encode dec =
    Json.Encode.list Basics.identity <|
        case dec of
            Let pub name_ expr ->
                [ Metadata.encode "Let" {}
                , Json.Encode.bool pub
                , Json.Encode.string name_
                , Expr.encode expr
                ]

            Ext pub name_ extName ->
                [ Metadata.encode "Ext" {}
                , Json.Encode.bool pub
                , Json.Encode.string name_
                , Json.Encode.string extName
                ]



-- UTILS -----------------------------------------------------------------------
