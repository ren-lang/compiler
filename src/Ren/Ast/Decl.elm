module Ren.Ast.Decl exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Decl.Meta as Meta exposing (Meta)
import Ren.Ast.Expr as Expr exposing (Expr)
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Decl
    = Let Meta Bool String Expr
    | Ext Meta Bool String String



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------
-- QUERIES ---------------------------------------------------------------------


name : Decl -> String
name dec =
    case dec of
        Let _ _ name_ _ ->
            name_

        Ext _ _ name_ _ ->
            name_


isPublic : Decl -> Bool
isPublic dec =
    case dec of
        Let _ pub _ _ ->
            pub

        Ext _ pub _ _ ->
            pub


isLocal : Decl -> Bool
isLocal dec =
    case dec of
        Let _ _ _ _ ->
            True

        Ext _ _ _ _ ->
            False


isExternal : Decl -> Bool
isExternal dec =
    case dec of
        Let _ _ _ _ ->
            False

        Ext _ _ _ _ ->
            True



-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------
-- JSON ------------------------------------------------------------------------


encode : Decl -> Json.Encode.Value
encode dec =
    case dec of
        Let meta pub name_ expr ->
            Json.taggedEncoder "Let"
                (Meta.encode meta)
                [ Json.Encode.bool pub
                , Json.Encode.string name_
                , Expr.encode expr
                ]

        Ext meta pub name_ extName ->
            Json.taggedEncoder "Ext"
                (Meta.encode meta)
                [ Json.Encode.bool pub
                , Json.Encode.string name_
                , Json.Encode.string extName
                ]


decoder : Json.Decode.Decoder Decl
decoder =
    Json.taggedDecoder
        (\key ->
            case key of
                "Let" ->
                    Json.Decode.map4 Let
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Json.Decode.bool)
                        (Json.Decode.index 2 <| Json.Decode.string)
                        (Json.Decode.index 3 <| Expr.decoder)

                "Ext" ->
                    Json.Decode.map4 Ext
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Json.Decode.bool)
                        (Json.Decode.index 2 <| Json.Decode.string)
                        (Json.Decode.index 3 <| Json.Decode.string)

                _ ->
                    Json.Decode.fail <| "Unknown declaration: " ++ key
        )



-- UTILS -----------------------------------------------------------------------
