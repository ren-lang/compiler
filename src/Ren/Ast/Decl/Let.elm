module Ren.Ast.Decl.Let exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Ast.Type as Type exposing (Type)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Span as Span exposing (Span)
import Ren.Data.Token as Token
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Let
    = Let Meta Bool String (Maybe Type) Expr


type alias Meta =
    { span : Span
    , tipe : Type
    , docs : List String
    }



-- QUERIES ---------------------------------------------------------------------


public : Let -> Bool
public (Let _ public_ _ _ _) =
    public_


name : Let -> String
name (Let _ _ name_ _ _) =
    name_


annotation : Let -> Maybe Type
annotation (Let _ _ _ annotation_ _) =
    annotation_


body : Let -> Expr
body (Let _ _ _ _ body_) =
    body_


meta : Let -> Meta
meta (Let meta_ _ _ _ _) =
    meta_


span : Let -> Span
span =
    meta >> .span


type_ : Let -> Type
type_ =
    meta >> .tipe


docs : Let -> List String
docs =
    meta >> .docs



-- MANIPULAtIONS ---------------------------------------------------------------


updateMeta : (Meta -> Meta) -> Let -> Let
updateMeta f (Let meta_ name_ public_ annotation_ body_) =
    Let (f meta_) name_ public_ annotation_ body_


setSpan : Span -> Let -> Let
setSpan span_ =
    updateMeta (\meta_ -> { meta_ | span = span_ })


setType : Type -> Let -> Let
setType tipe_ =
    updateMeta (\meta_ -> { meta_ | tipe = tipe_ })


setDocs : List String -> Let -> Let
setDocs docs_ =
    updateMeta (\meta_ -> { meta_ | docs = docs_ })


transformBody : (Expr -> Expr) -> Let -> Let
transformBody f (Let meta_ name_ public_ annotation_ body_) =
    Let meta_ name_ public_ annotation_ (f body_)



-- PARSING ---------------------------------------------------------------------


parser : List String -> Bool -> Parser () String Let
parser docs_ public_ =
    let
        makeLet name_ annotation_ body_ =
            Let
                { span = Span.empty, tipe = Type.Hole, docs = docs_ }
                public_
                name_
                annotation_
                body_
    in
    Parser.succeed makeLet
        |> Parser.drop (Parser.keyword "" Token.Let)
        |> Parser.keep (Parser.identifier "" Token.Lower)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.map Just annotationParser
                , Parser.succeed Nothing
                ]
            )
        |> Parser.drop (Parser.symbol "" Token.Equal)
        |> Parser.keep (Expr.parser { inArgPosition = False })
        |> Parser.withSpan setSpan


annotationParser : Parser () String Type
annotationParser =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol "" Token.Colon)
        |> Parser.keep (Type.parser { inArgPosition = False })



-- JSON ------------------------------------------------------------------------


encode : Let -> Json.Encode.Value
encode (Let meta_ public_ name_ annotation_ body_) =
    Json.taggedEncoder "Let" (encodeMeta meta_) <|
        [ Json.Encode.bool public_
        , Json.Encode.string name_
        , Maybe.withDefault Json.Encode.null <| Maybe.map Type.encode annotation_
        , Expr.encode body_
        ]


encodeMeta : Meta -> List ( String, Json.Encode.Value )
encodeMeta meta_ =
    [ ( "span", Span.encode meta_.span )
    , ( "type", Type.encode meta_.tipe )
    , ( "docs", Json.Encode.list Json.Encode.string meta_.docs )
    ]


decoder : Json.Decode.Decoder Let
decoder =
    Json.taggedDecoder
        (\tag ->
            if tag == "Let" then
                Json.Decode.map5 Let
                    (Json.Decode.index 0 <| metaDecoder)
                    (Json.Decode.index 1 <| Json.Decode.bool)
                    (Json.Decode.index 2 <| Json.Decode.string)
                    (Json.Decode.index 3 <| Json.Decode.nullable Type.decoder)
                    (Json.Decode.index 4 <| Expr.decoder)

            else
                Json.Decode.fail <| "Expected tag 'Let', but got '" ++ tag ++ "'"
        )


metaDecoder : Json.Decode.Decoder Meta
metaDecoder =
    Json.Decode.map3 Meta
        (Json.Decode.field "span" <| Span.decoder)
        (Json.Decode.field "type" <| Type.decoder)
        (Json.Decode.field "docs" <| Json.Decode.list Json.Decode.string)
