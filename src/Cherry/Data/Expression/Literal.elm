module Cherry.Data.Expression.Literal exposing 
    ( Literal(..)
    , coerceToNumber, coerceToInteger, coerceToString
    , decoder, primitiveDecoder
    , parser, primitiveParser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.Keywords as Keywords
import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))
import Parser.Extra


-- TYPES -----------------------------------------------------------------------


{-| -}
type Literal expression
    = Array (List expression)
    | Boolean Bool
    | Number Float
    | Object (Dict String expression)
    | String String


-- HELPERS ---------------------------------------------------------------------


{-| -}
coerceToNumber : Literal expression -> Maybe Float
coerceToNumber literal =
    case literal of
        Array _ ->
            Nothing

        Boolean True ->
            Just 1

        Boolean False ->
            Just 0

        Number f ->
            Just f

        Object _ ->
            Nothing

        String s ->
            String.toFloat s

{-| -}
coerceToInteger : Literal expression -> Maybe Int
coerceToInteger literal =
    let
        isIntegerFloat f i =
            Basics.toFloat i == f
    in
    case literal of
        Array _ ->
            Nothing

        Boolean True ->
            Just 1

        Boolean False ->
            Just 0

        Number f ->
            Basics.floor f |> \i ->
                if isIntegerFloat f i then
                    Just i

                else
                    Nothing

        Object _ ->
            Nothing

        String s ->
            String.toInt s

{-| -}
coerceToString : Literal expression -> Maybe String
coerceToString literal =
    case literal of
        Array _ ->
            Just "[array]"

        Boolean True ->
            Just "true"

        Boolean False ->
            Just "false"

        Number f ->
            Just (String.fromFloat f)

        Object _ ->
            Just "{object}"

        String s ->
            Just s


-- PARSING JSON ----------------------------------------------------------------


{-| -}
decoder : Decoder expression -> Decoder (Literal expression)
decoder expressionDecoder =
    Json.Decode.oneOf
        [ arrayLiteralDecoder expressionDecoder
        , booleanLiteralDecoder
        , objectLiteralDecoder expressionDecoder
        , numberLiteralDecoder
        , stringLiteralDecoder
        ]

{-| -}
primitiveDecoder : Decoder (Literal expression)
primitiveDecoder =
    Json.Decode.oneOf
        [ booleanLiteralDecoder
        , numberLiteralDecoder
        , stringLiteralDecoder
        ]

{-| -}
arrayLiteralDecoder : Decoder expression -> Decoder (Literal expression)
arrayLiteralDecoder expressionDecoder =
    Json.Decode.Extra.taggedObject "Literal.Array" <|
        Json.Decode.map Array
            (Json.Decode.field "elements" <|
                Json.Decode.list expressionDecoder
            )

{-| -}
booleanLiteralDecoder : Decoder (Literal expression)
booleanLiteralDecoder =
    Json.Decode.Extra.taggedObject "Literal.Boolean" <|
        Json.Decode.map Boolean
            (Json.Decode.field "boolean" Json.Decode.bool)

{-| -}
objectLiteralDecoder : Decoder expression -> Decoder (Literal expression)
objectLiteralDecoder expressionDecoder =
    Json.Decode.Extra.taggedObject "Literal.Object" <|
        Json.Decode.map Object
            (Json.Decode.field "fields" <|
                Json.Decode.dict expressionDecoder
            )

numberLiteralDecoder : Decoder (Literal expression)
numberLiteralDecoder =
    Json.Decode.Extra.taggedObject "Literal.Number" <|
        Json.Decode.map Number
            (Json.Decode.field "number" Json.Decode.float)

stringLiteralDecoder : Decoder (Literal expression)
stringLiteralDecoder =
    Json.Decode.Extra.taggedObject "Literal.String" <|
        Json.Decode.map String
            (Json.Decode.field "string" Json.Decode.string)


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : (String -> expression) -> Parser expression -> Parser (Literal expression)
parser toExpression expressionParser =
    Parser.oneOf
        [ arrayLiteralParser expressionParser
        , booleanLiteralParser
        , objectLiteralParser toExpression expressionParser
        , numberLiteralParser
        , stringLiteralParser
        ]

{-| -}
primitiveParser : Parser (Literal expression)
primitiveParser =
    Parser.oneOf
        [ booleanLiteralParser
        , numberLiteralParser
        , stringLiteralParser
        ]

{-| -}
arrayLiteralParser : Parser expression -> Parser (Literal expression)
arrayLiteralParser expressionParser =
    Parser.succeed Array
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , item = expressionParser
            , spaces = Parser.spaces
            , trailing = Parser.Forbidden
            }

{-| -}
booleanLiteralParser : Parser (Literal expression)
booleanLiteralParser =
    Parser.succeed Boolean
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.keyword "true"
            , Parser.succeed False
                |. Parser.keyword "false"
            ]

{-| -}
numberLiteralParser : Parser (Literal expression)
numberLiteralParser =
    let
        numberConfig =
            { int = Just Basics.toFloat
            , hex = Just Basics.toFloat
            , octal = Just Basics.toFloat
            , binary = Just Basics.toFloat
            , float = Just identity
            }
    in
    Parser.succeed Number
        |= Parser.oneOf
            [ Parser.succeed Basics.negate
                |. Parser.symbol "-" 
                |= Parser.number numberConfig
            , Parser.number numberConfig
            ]
        -- This is necessary to ensure we don't parse "123abc" as "Number 123"
        |. Parser.oneOf
            [ Parser.chompIf (Char.isAlpha)
                |> Parser.andThen (\_ -> Parser.problem "")
            , Parser.succeed ()
            ]

{-| -}
objectLiteralParser : (String -> expression) -> Parser expression -> Parser (Literal expression)
objectLiteralParser toExpression expressionParser =
    Parser.succeed (Dict.fromList >> Object)
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , item = 
                Parser.oneOf
                    [ Parser.succeed Tuple.pair
                        |= Parser.variable
                            { start = Char.isLower
                            , inner = Char.isAlphaNum
                            , reserved = Keywords.all
                            }
                        |. Parser.spaces
                        |. Parser.symbol ":"
                        |. Parser.spaces
                        |= expressionParser
                        |> Parser.backtrackable
                    , Parser.succeed (\name -> ( name, toExpression name ))
                        |= Parser.variable
                            { start = Char.isLower
                            , inner = Char.isAlphaNum
                            , reserved = Keywords.all
                            }
                    ]
            , spaces = Parser.spaces
            , trailing = Parser.Forbidden
            }

{-| -}
stringLiteralParser : Parser (Literal expression)
stringLiteralParser =
    Parser.succeed String
        |= Parser.oneOf
            [ Parser.Extra.string '"'
            , Parser.Extra.string '\''
            ]
