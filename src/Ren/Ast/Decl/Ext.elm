module Ren.Ast.Decl.Ext exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Type as Type exposing (Type)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Span as Span exposing (Span)
import Ren.Data.Token as Token
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Ext
    = Ext Meta Bool String (Maybe Type) String


type alias Meta =
    { span : Span
    , tipe : Type
    , docs : List String
    }



-- CONSTRUCTORS ----------------------------------------------------------------


new : Bool -> String -> Maybe Type -> String -> Ext
new public_ name_ tipe_ body_ =
    Ext
        { span = Span.empty
        , tipe = Maybe.withDefault Type.Any tipe_
        , docs = []
        }
        public_
        name_
        tipe_
        body_



-- QUERIES ---------------------------------------------------------------------


public : Ext -> Bool
public (Ext _ public_ _ _ _) =
    public_


name : Ext -> String
name (Ext _ _ name_ _ _) =
    name_


annotation : Ext -> Maybe Type
annotation (Ext _ _ _ annotation_ _) =
    annotation_


body : Ext -> String
body (Ext _ _ _ _ body_) =
    body_


meta : Ext -> Meta
meta (Ext meta_ _ _ _ _) =
    meta_


span : Ext -> Span
span =
    meta >> .span


type_ : Ext -> Type
type_ =
    meta >> .tipe


docs : Ext -> List String
docs =
    meta >> .docs



-- MANIPULAtIONS ---------------------------------------------------------------


updateMeta : (Meta -> Meta) -> Ext -> Ext
updateMeta f (Ext meta_ public_ name_ annotation_ body_) =
    Ext (f meta_) public_ name_ annotation_ body_


setSpan : Span -> Ext -> Ext
setSpan span_ =
    updateMeta (\meta_ -> { meta_ | span = span_ })


setType : Type -> Ext -> Ext
setType tipe_ =
    updateMeta (\meta_ -> { meta_ | tipe = tipe_ })


setDocs : List String -> Ext -> Ext
setDocs docs_ =
    updateMeta (\meta_ -> { meta_ | docs = docs_ })



-- PARSING ---------------------------------------------------------------------


parser : List String -> Bool -> Parser () String Ext
parser docs_ public_ =
    let
        makeExt name_ annotation_ body_ =
            Ext
                { span = Span.empty, tipe = Type.Hole, docs = docs_ }
                public_
                name_
                annotation_
                body_
    in
    Parser.succeed makeExt
        |> Parser.drop (Parser.keyword "" Token.Ext)
        |> Parser.keep (Parser.identifier "" Token.Lower)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.map Just annotationParser
                , Parser.succeed Nothing
                ]
            )
        |> Parser.drop (Parser.symbol "" Token.Equal)
        |> Parser.keep (Parser.string "")
        |> Parser.withSpan setSpan


annotationParser : Parser () String Type
annotationParser =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol "" Token.Colon)
        |> Parser.keep (Type.parser { inArgPosition = False })



-- JSON ------------------------------------------------------------------------


encode : Ext -> Json.Encode.Value
encode (Ext meta_ public_ name_ annotation_ body_) =
    Json.taggedEncoder "Ext" (encodeMeta meta_) <|
        [ Json.Encode.bool public_
        , Json.Encode.string name_
        , Maybe.withDefault Json.Encode.null <| Maybe.map Type.encode annotation_
        , Json.Encode.string body_
        ]


encodeMeta : Meta -> List ( String, Json.Encode.Value )
encodeMeta meta_ =
    [ ( "span", Span.encode meta_.span )
    , ( "type", Type.encode meta_.tipe )
    , ( "docs", Json.Encode.list Json.Encode.string meta_.docs )
    ]


decoder : Json.Decode.Decoder Ext
decoder =
    Json.taggedDecoder
        (\tag ->
            if tag == "Ext" then
                Json.Decode.map5 Ext
                    (Json.Decode.index 0 <| metaDecoder)
                    (Json.Decode.index 1 <| Json.Decode.bool)
                    (Json.Decode.index 2 <| Json.Decode.string)
                    (Json.Decode.index 3 <| Json.Decode.nullable Type.decoder)
                    (Json.Decode.index 4 <| Json.Decode.string)

            else
                Json.Decode.fail <| "Expected tag 'Ext', but got '" ++ tag ++ "'"
        )


metaDecoder : Json.Decode.Decoder Meta
metaDecoder =
    Json.Decode.map3 Meta
        (Json.Decode.field "span" <| Span.decoder)
        (Json.Decode.field "type" <| Type.decoder)
        (Json.Decode.field "docs" <| Json.Decode.list Json.Decode.string)
