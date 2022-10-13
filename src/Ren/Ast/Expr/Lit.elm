module Ren.Ast.Expr.Lit exposing
    ( Lit(..)
    , ParseContext, Parsers, parser
    , encode, decoder
    )

{-|


## Types

@docs Lit


## Parsing

@docs ParseContext, Parsers, parser


## JSON

@docs encode, decoder

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Token as Token
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


{-| -}
type Lit expr
    = Array (List expr)
    | Enum String (List expr)
    | Number Float
    | Record (List ( String, expr ))
    | String String


{-| -}
type alias ParseContext =
    { inArgPosition : Bool }


{-| -}
type alias Parsers a =
    { fromString : String -> a
    , itemParser : Parser () String a
    , wrapParser : Parser () String a
    }



-- PARSERS ---------------------------------------------------------------------


{-| -}
parser : ParseContext -> Parsers a -> Parser () String (Lit a)
parser context itemParsers =
    Parser.oneOf
        [ arrayParser itemParsers
        , enumParser context itemParsers
        , numberParser
        , recordParser itemParsers
        , stringParser
        ]


arrayParser : Parsers a -> Parser () String (Lit a)
arrayParser { itemParser } =
    let
        elements =
            Parser.loop []
                (\els ->
                    Parser.oneOf
                        [ Parser.succeed (\el -> el :: els)
                            |> Parser.drop (Parser.symbol "" <| Token.Comma)
                            |> Parser.keep itemParser
                            |> Parser.map Parser.Continue
                        , Parser.succeed (\_ -> List.reverse els)
                            |> Parser.keep (Parser.symbol "" <| Token.Bracket Token.Right)
                            |> Parser.map Parser.Break
                        ]
                )
    in
    Parser.succeed Array
        |> Parser.drop (Parser.symbol "" <| Token.Bracket Token.Left)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.succeed (::)
                    |> Parser.keep itemParser
                    |> Parser.keep elements
                , Parser.succeed []
                    |> Parser.drop (Parser.symbol "" <| Token.Bracket Token.Right)
                ]
            )


enumParser : ParseContext -> Parsers a -> Parser () String (Lit a)
enumParser { inArgPosition } { wrapParser } =
    let
        args =
            Parser.many wrapParser
    in
    Parser.succeed Enum
        |> Parser.drop (Parser.symbol "" <| Token.Colon)
        |> Parser.keep (Parser.identifier "" Token.Lower)
        |> Parser.andThen
            (\con ->
                if inArgPosition then
                    Parser.succeed <| con []

                else
                    Parser.map con args
            )


numberParser : Parser () String (Lit a)
numberParser =
    Parser.succeed Number
        |> Parser.keep (Parser.number "")


recordParser : Parsers a -> Parser () String (Lit a)
recordParser { fromString, itemParser } =
    let
        field =
            Parser.succeed (\key val -> ( key, Maybe.withDefault (fromString key) val ))
                |> Parser.keep (Parser.identifier "" Token.Lower)
                |> Parser.keep
                    (Parser.oneOf
                        [ Parser.succeed Just
                            |> Parser.drop (Parser.symbol "" <| Token.Colon)
                            |> Parser.keep itemParser
                        , Parser.succeed Nothing
                        ]
                    )

        fields =
            Parser.loop []
                (\fs ->
                    Parser.oneOf
                        [ Parser.succeed (\f -> f :: fs)
                            |> Parser.drop (Parser.symbol "" Token.Comma)
                            |> Parser.keep field
                            |> Parser.map Parser.Continue
                        , Parser.succeed (\_ -> List.reverse fs)
                            |> Parser.keep (Parser.symbol "" <| Token.Brace Token.Right)
                            |> Parser.map Parser.Break
                        ]
                )
    in
    Parser.succeed Record
        |> Parser.drop (Parser.symbol "" <| Token.Brace Token.Left)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.succeed (::)
                    |> Parser.keep field
                    |> Parser.keep fields
                , Parser.succeed []
                    |> Parser.drop (Parser.symbol "" <| Token.Brace Token.Right)
                ]
            )


stringParser : Parser () String (Lit a)
stringParser =
    Parser.succeed String
        |> Parser.keep (Parser.string "")



-- JSON ------------------------------------------------------------------------


{-| -}
encode : (expr -> Json.Encode.Value) -> Lit expr -> Json.Encode.Value
encode encodeExpr literal =
    let
        encodeField ( k, v ) =
            Json.taggedEncoder "Field"
                []
                [ Json.Encode.string k
                , encodeExpr v
                ]
    in
    case literal of
        Array elements ->
            Json.taggedEncoder "Array"
                []
                [ Json.Encode.list encodeExpr elements
                ]

        Enum tag elements ->
            Json.taggedEncoder "Enum"
                []
                [ Json.Encode.string tag
                , Json.Encode.list encodeExpr elements
                ]

        Number n ->
            Json.taggedEncoder "Number"
                []
                [ Json.Encode.float n
                ]

        Record fields ->
            Json.taggedEncoder "Record"
                []
                [ Json.Encode.list encodeField fields
                ]

        String s ->
            Json.taggedEncoder "String"
                []
                [ Json.Encode.string s
                ]


{-| -}
decoder : Json.Decode.Decoder expr -> Json.Decode.Decoder (Lit expr)
decoder exprDecoder =
    let
        fieldDecoder =
            Json.taggedDecoder
                (\key ->
                    if key == "Field" then
                        Json.Decode.map2 Tuple.pair
                            (Json.Decode.index 1 <| Json.Decode.string)
                            (Json.Decode.index 2 <| exprDecoder)

                    else
                        Json.Decode.fail <| "Unknown record field: " ++ key
                )
    in
    Json.taggedDecoder
        (\key ->
            case key of
                "Array" ->
                    Json.Decode.map Array
                        (Json.Decode.index 1 <| Json.Decode.list exprDecoder)

                "Enum" ->
                    Json.Decode.map2 Enum
                        (Json.Decode.index 1 <| Json.Decode.string)
                        (Json.Decode.index 2 <| Json.Decode.list exprDecoder)

                "Number" ->
                    Json.Decode.map Number
                        (Json.Decode.index 1 <| Json.Decode.float)

                "Record" ->
                    Json.Decode.map Record
                        (Json.Decode.index 1 <| Json.Decode.list fieldDecoder)

                "String" ->
                    Json.Decode.map String
                        (Json.Decode.index 1 <| Json.Decode.string)

                _ ->
                    Json.Decode.fail <| "Unknown literal type: " ++ key
        )
