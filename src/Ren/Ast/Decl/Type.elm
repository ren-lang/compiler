module Ren.Ast.Decl.Type exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Type as Type
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Span as Span exposing (Span)
import Ren.Data.Token as Token
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Type
    = Type Meta Bool String (List String) Type.Type


type alias Meta =
    { span : Span
    , docs : List String
    }



-- QUERIES ---------------------------------------------------------------------


name : Type -> String
name (Type _ _ name_ _ _) =
    name_


public : Type -> Bool
public (Type _ public_ _ _ _) =
    public_


parameters : Type -> List String
parameters (Type _ _ _ parameters_ _) =
    parameters_


body : Type -> Type.Type
body (Type _ _ _ _ body_) =
    body_


meta : Type -> Meta
meta (Type meta_ _ _ _ _) =
    meta_


span : Type -> Span
span =
    meta >> .span


docs : Type -> List String
docs =
    meta >> .docs



-- MANIPULAtIONS ---------------------------------------------------------------


updateMeta : (Meta -> Meta) -> Type -> Type
updateMeta f (Type meta_ name_ public_ annotation_ body_) =
    Type (f meta_) name_ public_ annotation_ body_


setSpan : Span -> Type -> Type
setSpan span_ =
    updateMeta (\meta_ -> { meta_ | span = span_ })


setDocs : List String -> Type -> Type
setDocs docs_ =
    updateMeta (\meta_ -> { meta_ | docs = docs_ })



-- PARSING ---------------------------------------------------------------------


parser : List String -> Bool -> Parser () String Type
parser docs_ public_ =
    let
        makeType name_ parameters_ body_ =
            Type
                { span = Span.empty, docs = docs_ }
                public_
                name_
                parameters_
                body_
    in
    Parser.succeed makeType
        |> Parser.drop (Parser.keyword "" Token.Type)
        |> Parser.keep (Parser.identifier "" Token.Upper)
        |> Parser.keep (Parser.many (Parser.identifier "" Token.Lower))
        |> Parser.drop (Parser.symbol "" Token.Equal)
        |> Parser.keep (Type.parser { inArgPosition = False })



-- JSON ------------------------------------------------------------------------


encode : Type -> Json.Encode.Value
encode (Type meta_ public_ name_ parameters_ body_) =
    Json.taggedEncoder "Type" (encodeMeta meta_) <|
        [ Json.Encode.bool public_
        , Json.Encode.string name_
        , Json.Encode.list Json.Encode.string parameters_
        , Type.encode body_
        ]


encodeMeta : Meta -> List ( String, Json.Encode.Value )
encodeMeta meta_ =
    [ ( "span", Span.encode meta_.span )
    , ( "docs", Json.Encode.list Json.Encode.string meta_.docs )
    ]


decoder : Json.Decode.Decoder Type
decoder =
    Json.taggedDecoder
        (\tag ->
            if tag == "Type" then
                Json.Decode.map5 Type
                    (Json.Decode.index 0 <| metaDecoder)
                    (Json.Decode.index 2 <| Json.Decode.bool)
                    (Json.Decode.index 1 <| Json.Decode.string)
                    (Json.Decode.index 3 <| Json.Decode.list Json.Decode.string)
                    (Json.Decode.index 4 <| Type.decoder)

            else
                Json.Decode.fail <| "Expected tag 'Type', but got '" ++ tag ++ "'"
        )


metaDecoder : Json.Decode.Decoder Meta
metaDecoder =
    Json.Decode.map2 Meta
        (Json.Decode.field "span" <| Span.decoder)
        (Json.Decode.field "docs" <| Json.Decode.list Json.Decode.string)
