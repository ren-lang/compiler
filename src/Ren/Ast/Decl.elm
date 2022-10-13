module Ren.Ast.Decl exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Decl.Ext as Ext exposing (Ext)
import Ren.Ast.Decl.Imp as Imp exposing (Imp)
import Ren.Ast.Decl.Let as Let exposing (Let)
import Ren.Ast.Decl.Type as Type exposing (Type)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Span as Span exposing (Span)
import Ren.Data.Token as Token
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Decl
    = Let Let
    | Ext Ext
    | Imp Imp
    | Type Type
    | Comment Span (List String)



-- CONSTRUCTORS ----------------------------------------------------------------
-- QUERIES ---------------------------------------------------------------------


isLet : Decl -> Bool
isLet decl =
    case decl of
        Let _ ->
            True

        _ ->
            False


isExt : Decl -> Bool
isExt decl =
    case decl of
        Ext _ ->
            True

        _ ->
            False


isImp : Decl -> Bool
isImp decl =
    case decl of
        Imp _ ->
            True

        _ ->
            False


isType : Decl -> Bool
isType decl =
    case decl of
        Type _ ->
            True

        _ ->
            False


isComment : Decl -> Bool
isComment decl =
    case decl of
        Comment _ _ ->
            True

        _ ->
            False


span : Decl -> Span
span decl =
    case decl of
        Let let_ ->
            Let.span let_

        Ext ext ->
            Ext.span ext

        Imp imp ->
            Imp.span imp

        Type typ ->
            Type.span typ

        Comment span_ _ ->
            span_



-- MANIPULATIONS ---------------------------------------------------------------


setSpan : Span -> Decl -> Decl
setSpan span_ decl =
    case decl of
        Let let_ ->
            Let <| Let.setSpan span_ let_

        Ext ext ->
            Ext <| Ext.setSpan span_ ext

        Imp imp ->
            Imp <| Imp.setSpan span_ imp

        Type typ ->
            Type <| Type.setSpan span_ typ

        Comment _ comment ->
            Comment span_ comment



-- CONVERSIONS -----------------------------------------------------------------


asLet : Decl -> Maybe Let
asLet decl =
    case decl of
        Let let_ ->
            Just let_

        _ ->
            Nothing


asExt : Decl -> Maybe Ext
asExt decl =
    case decl of
        Ext ext ->
            Just ext

        _ ->
            Nothing


asImp : Decl -> Maybe Imp
asImp decl =
    case decl of
        Imp imp ->
            Just imp

        _ ->
            Nothing


asType : Decl -> Maybe Type
asType decl =
    case decl of
        Type typ ->
            Just typ

        _ ->
            Nothing


asComment : Decl -> Maybe ( Span, List String )
asComment decl =
    case decl of
        Comment span_ comment ->
            Just ( span_, comment )

        _ ->
            Nothing



-- PARSING ---------------------------------------------------------------------


parser : Parser () String Decl
parser =
    Parser.withSpan setSpan
        (Parser.oneOf
            [ Parser.map Imp Imp.parser
            , declWithDocComments
            , Parser.withSpan Comment commentParser
            ]
        )


declWithDocComments : Parser () String Decl
declWithDocComments =
    Parser.do docCommentParser <| \docs ->
    Parser.do visibilityParser <| \isPublic ->
    Parser.oneOf
        [ Parser.map Let <| Let.parser docs isPublic
        , Parser.map Ext <| Ext.parser docs isPublic
        , Parser.map Type <| Type.parser docs isPublic
        ]


visibilityParser : Parser () String Bool
visibilityParser =
    Parser.oneOf
        [ Parser.succeed True
            |> Parser.drop (Parser.keyword "" Token.Pub)
        , Parser.succeed False
        ]


docCommentParser : Parser () String (List String)
docCommentParser =
    Parser.oneOf
        [ Parser.succeed (::)
            |> Parser.keep (Parser.docComment "")
            |> Parser.keep (Parser.many (Parser.oneOf [ Parser.docComment "", Parser.comment "" ]))
        , Parser.succeed []
        ]


commentParser : Parser () String (List String)
commentParser =
    Parser.succeed (::)
        |> Parser.keep (Parser.comment "")
        |> Parser.keep (Parser.many (Parser.comment ""))



-- JSON ------------------------------------------------------------------------


encode : Decl -> Json.Encode.Value
encode decl =
    case decl of
        Let let_ ->
            Let.encode let_

        Ext ext ->
            Ext.encode ext

        Imp imp ->
            Imp.encode imp

        Type typ ->
            Type.encode typ

        Comment span_ comments ->
            Json.taggedEncoder "Comment" [ ( "span", Span.encode span_ ) ] <|
                [ Json.Encode.list Json.Encode.string comments ]


decoder : Json.Decode.Decoder Decl
decoder =
    Json.Decode.oneOf
        [ Json.Decode.map Let Let.decoder
        , Json.Decode.map Ext Ext.decoder
        , Json.Decode.map Imp Imp.decoder
        , Json.Decode.map Type Type.decoder
        , Json.taggedDecoder
            (\tag ->
                if tag == "Comment" then
                    Json.Decode.map2 Comment
                        (Json.Decode.index 0 <| Json.Decode.field "span" Span.decoder)
                        (Json.Decode.index 1 <| Json.Decode.list Json.Decode.string)

                else
                    Json.Decode.fail <| "Expected tag 'Comment', but got '" ++ tag ++ "'"
            )
        ]
