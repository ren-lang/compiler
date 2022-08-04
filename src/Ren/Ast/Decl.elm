module Ren.Ast.Decl exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Decl.Meta as Meta
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Token as Token
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Decl
    = Let Meta Bool String Expr
    | Ext Meta Bool String String


type alias Meta =
    Meta.Meta



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


local : Meta -> Bool -> String -> Expr -> Decl
local meta pub name_ expr =
    Let meta pub name_ expr


external : Meta -> Bool -> String -> String -> Decl
external meta pub name_ extName =
    Ext meta pub name_ extName



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


transformMeta : (Meta -> Meta) -> Decl -> Decl
transformMeta f decl =
    case decl of
        Let meta pub name_ expr ->
            Let (f meta) pub name_ expr

        Ext meta pub name_ extName ->
            Ext (f meta) pub name_ extName



-- CONVERSIONS -----------------------------------------------------------------


toJson : Decl -> String
toJson =
    encode >> Json.Encode.encode 4



-- PARSING ---------------------------------------------------------------------


parser : Parser () String Decl
parser =
    visibilityParser
        |> Parser.andThen
            (\pub ->
                Parser.oneOf
                    [ Parser.succeed (local Meta.default pub)
                        |> Parser.drop (Parser.keyword "" Token.Let)
                        |> Parser.keep (Parser.identifier "" Token.Lower)
                        |> Parser.drop (Parser.symbol "" Token.Equal)
                        |> Parser.keep (Parser.map Expr.desugar <| Expr.parser { inArgPosition = False })
                    , Parser.succeed (external Meta.default pub)
                        |> Parser.drop (Parser.keyword "" Token.Ext)
                        |> Parser.keep (Parser.identifier "" Token.Lower)
                        |> Parser.drop (Parser.symbol "" Token.Equal)
                        |> Parser.keep (Parser.string "")
                    ]
            )
        |> withParseMetadata


visibilityParser : Parser () String Bool
visibilityParser =
    Parser.oneOf
        [ Parser.succeed True |> Parser.drop (Parser.keyword "" Token.Pub)
        , Parser.succeed False
        ]



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


withParseMetadata : Parser () String Decl -> Parser () String Decl
withParseMetadata =
    let
        addComments comments expr =
            transformMeta (\metadata -> List.foldr Meta.addComment metadata comments) expr

        addSpan span expr =
            transformMeta (\metadata -> Meta.setSpan span metadata) expr
    in
    Parser.withComments "" addComments >> Parser.withSpan addSpan
