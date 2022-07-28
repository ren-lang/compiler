module Ren.Ast.Expr.Pat exposing (..)

{-|


## Types

@docs Pattern


## Parsing

@docs ParseContext, parser


## JSON

@docs encode, decoder

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Expr.Lit as Lit exposing (Lit)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Token as Token
import Set exposing (Set)
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


{-| -}
type Pat
    = Any
    | Literal (Lit Pat)
    | Spread String
    | Type String Pat
    | Var String


{-| -}
type alias ParseContext =
    { inArgPosition : Bool
    , includeSpread : Bool
    }



-- CONSTANTS -------------------------------------------------------------------


parseContext : ParseContext
parseContext =
    { inArgPosition = False
    , includeSpread = False
    }



-- QUERIES ---------------------------------------------------------------------


{-| Check if a particular variable name is bound by a pattern. Names are bound
no matter how deeply nested a pattern is, so this recursively checks any nested
patterns too.
-}
binds : String -> Pat -> Bool
binds name pattern =
    case pattern of
        Any ->
            False

        Literal (Lit.Array array) ->
            List.any (binds name) array

        Literal (Lit.Enum _ enum) ->
            List.any (binds name) enum

        Literal (Lit.Record record) ->
            List.any (Tuple.second >> binds name) record

        Literal _ ->
            False

        Spread n ->
            n == name

        Type _ pat ->
            binds name pat

        Var n ->
            n == name


bindings : Pat -> Set String
bindings pattern =
    case pattern of
        Any ->
            Set.empty

        Literal (Lit.Array array) ->
            List.foldl (bindings >> Set.union) Set.empty array

        Literal (Lit.Enum _ enum) ->
            List.foldl (bindings >> Set.union) Set.empty enum

        Literal (Lit.Record record) ->
            List.foldl (Tuple.second >> bindings >> Set.union) Set.empty record

        Literal _ ->
            Set.empty

        Spread name ->
            Set.singleton name

        Type _ pat ->
            bindings pat

        Var name ->
            Set.singleton name



-- PARSERS ---------------------------------------------------------------------


{-| -}
parser : ParseContext -> Parser () String Pat
parser context =
    Parser.oneOf <|
        List.concat
            [ [ anyParser
              , literalParser context
              , typeParser context
              , varParser
              ]
            , if context.includeSpread then
                [ spreadParser ]

              else
                []
            , [ parenthesisedParser context ]
            ]


anyParser : Parser () String Pat
anyParser =
    Parser.succeed Any
        |> Parser.drop (Parser.symbol "" Token.Underscore)


literalParser : ParseContext -> Parser () String Pat
literalParser context =
    Parser.succeed Literal
        |> Parser.keep
            (Lit.parser
                { inArgPosition = context.inArgPosition }
                { fromString = Var
                , itemParser = Parser.lazy <| \_ -> parser { context | includeSpread = True }
                , wrapParser = Parser.lazy <| \_ -> parser context
                }
            )


parenthesisedParser : ParseContext -> Parser () String Pat
parenthesisedParser context =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol "" <| Token.Paren Token.Left)
        |> Parser.keep (Parser.lazy <| \_ -> parser { context | inArgPosition = False })
        |> Parser.drop (Parser.symbol "" <| Token.Paren Token.Right)


spreadParser : Parser () String Pat
spreadParser =
    Parser.succeed Spread
        |> Parser.drop (Parser.symbol "" <| Token.Period)
        |> Parser.drop (Parser.symbol "" <| Token.Period)
        |> Parser.drop (Parser.symbol "" <| Token.Period)
        |> Parser.keep (Parser.identifier "" Token.Lower)


typeParser : ParseContext -> Parser () String Pat
typeParser context =
    Parser.succeed Type
        |> Parser.drop (Parser.symbol "" Token.At)
        |> Parser.keep (Parser.identifier "" Token.Upper)
        |> Parser.keep (Parser.lazy <| \_ -> parser { context | inArgPosition = True })


varParser : Parser () String Pat
varParser =
    Parser.succeed Var
        |> Parser.keep (Parser.identifier "" Token.Lower)



-- JSON ------------------------------------------------------------------------


{-| -}
encode : Pat -> Json.Encode.Value
encode pattern =
    case pattern of
        Any ->
            Json.taggedEncoder "Any" [] []

        Literal literal ->
            Json.taggedEncoder "Literal"
                []
                [ Lit.encode encode literal
                ]

        Spread name ->
            Json.taggedEncoder "Spread"
                []
                [ Json.Encode.string name
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


{-| -}
decoder : Json.Decode.Decoder Pat
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

                "Spread" ->
                    Json.Decode.map Spread
                        (Json.Decode.index 1 <| Json.Decode.string)

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
