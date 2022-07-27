module Ren.Ast.Expr.Pat exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Expr.Lit as Lit exposing (Lit)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Token as Token
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Pattern
    = Any
    | Literal (Lit Pattern)
    | Type String Pattern
    | Var String


type alias Context r =
    { r | inArgPosition : Bool }



-- PARSERS ---------------------------------------------------------------------


parser : Context r -> Parser () () Pattern
parser context =
    Parser.oneOf
        [ parenthesisedParser context
        , anyParser
        , literalParser context
        , typeParser context
        , varParser
        ]


anyParser : Parser () () Pattern
anyParser =
    Parser.succeed Any
        |> Parser.drop (Parser.symbol () Token.Underscore)


literalParser : Context r -> Parser () () Pattern
literalParser context =
    Parser.succeed Literal
        |> Parser.keep
            (Lit.parser context
                { fromString = Var
                , itemParser = Parser.lazy <| \_ -> parser context
                , wrapParser = Parser.lazy <| \_ -> parser context
                }
            )


parenthesisedParser : Context r -> Parser () () Pattern
parenthesisedParser context =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol () <| Token.Paren Token.Left)
        |> Parser.keep (Parser.lazy <| \_ -> parser { context | inArgPosition = False })
        |> Parser.drop (Parser.symbol () <| Token.Paren Token.Right)


typeParser : Context r -> Parser () () Pattern
typeParser context =
    Parser.succeed Type
        |> Parser.drop (Parser.symbol () Token.At)
        |> Parser.keep (Parser.identifier () Token.Upper)
        |> Parser.keep (Parser.lazy <| \_ -> parser { context | inArgPosition = True })


varParser : Parser () () Pattern
varParser =
    Parser.succeed Var
        |> Parser.keep (Parser.identifier () Token.Lower)



-- JSON ------------------------------------------------------------------------


encode : Pattern -> Json.Encode.Value
encode pattern =
    case pattern of
        Any ->
            Json.taggedEncoder "Any" [] []

        Literal literal ->
            Json.taggedEncoder "Literal"
                []
                [ Lit.encode encode literal
                ]

        Type t pat ->
            Json.taggedEncoder "Type"
                []
                [ Json.Encode.string t
                , encode pat
                ]

        Var name ->
            Json.taggedEncoder "Var"
                []
                [ Json.Encode.string name
                ]


decoder : Json.Decode.Decoder Pattern
decoder =
    let
        lazyDecoder =
            Json.Decode.lazy <| \_ -> decoder
    in
    Json.taggedDecoder
        (\key ->
            case key of
                "Any" ->
                    Json.Decode.succeed Any

                "Literal" ->
                    Json.Decode.map Literal
                        (Json.Decode.index 1 <| Lit.decoder <| lazyDecoder)

                "Type" ->
                    Json.Decode.map2 Type
                        (Json.Decode.index 1 <| Json.Decode.string)
                        (Json.Decode.index 2 <| lazyDecoder)

                "Var" ->
                    Json.Decode.map Var
                        (Json.Decode.index 1 <| Json.Decode.string)

                _ ->
                    Json.Decode.fail <| "Unknown pattern type: " ++ key
        )
