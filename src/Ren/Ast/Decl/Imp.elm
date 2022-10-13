module Ren.Ast.Decl.Imp exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Span as Span exposing (Span)
import Ren.Data.Token as Token
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Imp
    = Imp Meta String Source (List String)


type Source
    = External
    | Package
    | Relative


type alias Meta =
    { span : Span
    }



-- QUERIES ---------------------------------------------------------------------


path : Imp -> String
path (Imp _ path_ _ _) =
    path_


source : Imp -> Source
source (Imp _ _ source_ _) =
    source_


qualified : Imp -> List String
qualified (Imp _ _ _ qualified_) =
    qualified_


alike : Imp -> Imp -> Bool
alike (Imp _ aPath aSource _) (Imp _ bPath bSource _) =
    aPath == bPath && aSource == bSource


meta : Imp -> Meta
meta (Imp meta_ _ _ _) =
    meta_


isLocal : Imp -> Bool
isLocal =
    source >> (==) Relative


isPkg : Imp -> Bool
isPkg =
    source >> (==) Package


isExt : Imp -> Bool
isExt =
    source >> (==) External


span : Imp -> Span
span =
    meta >> .span



-- MANIPULATIONS ---------------------------------------------------------------


updateMeta : (Meta -> Meta) -> Imp -> Imp
updateMeta f (Imp meta_ path_ source_ qualified_) =
    Imp (f meta_) path_ source_ qualified_


setSpan : Span -> Imp -> Imp
setSpan span_ =
    updateMeta (\meta_ -> { meta_ | span = span_ })



-- PARSING ---------------------------------------------------------------------


parser : Parser () String Imp
parser =
    let
        makeImp source_ path_ qualified_ =
            Imp { span = Span.empty } path_ source_ qualified_
    in
    Parser.succeed makeImp
        |> Parser.drop (Parser.keyword "" Token.Import)
        |> Parser.keep sourceParser
        |> Parser.keep (Parser.string "")
        |> Parser.keep (Parser.oneOf [ qualifiedParser, Parser.succeed [] ])
        |> Parser.withSpan setSpan


sourceParser : Parser () String Source
sourceParser =
    Parser.oneOf
        [ Parser.succeed External |> Parser.drop (Parser.keyword "" Token.Ext)
        , Parser.succeed Package |> Parser.drop (Parser.keyword "" Token.Pkg)
        , Parser.succeed Relative
        ]


qualifiedParser : Parser () String (List String)
qualifiedParser =
    Parser.succeed (::)
        |> Parser.drop (Parser.keyword "" Token.As)
        |> Parser.keep (Parser.identifier "" Token.Upper)
        |> Parser.keep
            (Parser.many
                (Parser.succeed Basics.identity
                    |> Parser.drop (Parser.symbol "" Token.Period)
                    |> Parser.keep (Parser.identifier "" Token.Upper)
                )
            )



-- JSON ------------------------------------------------------------------------


encode : Imp -> Json.Encode.Value
encode (Imp meta_ path_ source_ qualified_) =
    Json.taggedEncoder "Imp" (encodeMeta meta_) <|
        [ Json.Encode.string path_
        , encodeSource source_
        , Json.Encode.list Json.Encode.string qualified_
        ]


encodeMeta : Meta -> List ( String, Json.Encode.Value )
encodeMeta meta_ =
    [ ( "span", Span.encode meta_.span ) ]


encodeSource : Source -> Json.Encode.Value
encodeSource source_ =
    case source_ of
        External ->
            Json.Encode.string "External"

        Package ->
            Json.Encode.string "Package"

        Relative ->
            Json.Encode.string "Relative"


decoder : Json.Decode.Decoder Imp
decoder =
    Json.taggedDecoder
        (\tag ->
            if tag == "Imp" then
                Json.Decode.map4 Imp
                    (Json.Decode.index 0 <| metaDecoder)
                    (Json.Decode.index 1 <| Json.Decode.string)
                    (Json.Decode.index 2 <| sourceDecoder)
                    (Json.Decode.index 3 <| Json.Decode.list Json.Decode.string)

            else
                Json.Decode.fail <| "Expected tag 'Let', but got '" ++ tag ++ "'"
        )


metaDecoder : Json.Decode.Decoder Meta
metaDecoder =
    Json.Decode.map Meta
        (Json.Decode.field "span" <| Span.decoder)


sourceDecoder : Json.Decode.Decoder Source
sourceDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\source_ ->
                if source_ == "External" then
                    Json.Decode.succeed External

                else if source_ == "Package" then
                    Json.Decode.succeed Package

                else if source_ == "Relative" then
                    Json.Decode.succeed Relative

                else
                    Json.Decode.fail <| "Expected 'External', 'Package' or 'Relative', but got '" ++ source_ ++ "'"
            )
