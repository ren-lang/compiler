module Ren.Compiler.Lex exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Parser.Advanced as Parser exposing ((|.), (|=))
import Ren.AST.Token as Token exposing (Token(..))
import Ren.Compiler.Error as Error exposing (Error)
import Ren.Compiler.Parse.Util as Util
import Ren.Data.Span as Span exposing (Span)
import Set exposing (Set)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Parser a =
    Parser.Parser Error.ParseContext () a


type alias Stream =
    List ( Span, Token )



--


run : String -> Result Error Stream
run input =
    input
        |> Parser.run
            (Util.many (token |. Util.whitespace)
                |. Parser.end ()
            )
        |> Result.mapError (\_ -> Error.LexError)



--


token : Parser ( Span, Token )
token =
    Span.parser Tuple.pair <|
        Parser.oneOf
            [ Parser.backtrackable keyword
            , Parser.backtrackable symbol
            , Parser.backtrackable comment
            , Parser.backtrackable operator
            , Parser.backtrackable literal
            , Parser.backtrackable identifier
            , Parser.chompIf (Basics.always True) ()
                |> Parser.getChompedString
                |> Parser.map Token.Unknown
            ]


keyword : Parser Token
keyword =
    let
        keyword_ s =
            Parser.keyword (Parser.Token s ())
                |> Parser.getChompedString
                |> Parser.andThen
                    (Token.keyword >> Maybe.map Parser.succeed >> Maybe.withDefault (Parser.problem ()))
    in
    Set.toList Token.keywords
        |> List.map keyword_
        |> Parser.oneOf


symbol : Parser Token
symbol =
    let
        symbol_ s =
            Parser.symbol (Parser.Token s ())
                |> Parser.getChompedString
                |> Parser.andThen
                    (Token.symbol >> Maybe.map Parser.succeed >> Maybe.withDefault (Parser.problem ()))
    in
    Set.toList Token.symbols
        |> List.map symbol_
        |> Parser.oneOf


comment : Parser Token
comment =
    let
        tok =
            Parser.Token "//" ()
    in
    Parser.lineComment tok
        |> Parser.getChompedString
        |> Parser.map Token.Comment


operator : Parser Token
operator =
    Parser.chompUntil (Parser.Token " " ())
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                case Token.operator s of
                    Just op ->
                        Parser.succeed op

                    Nothing ->
                        Parser.problem ()
            )


literal : Parser Token
literal =
    Parser.oneOf
        [ Parser.succeed (Token.Boolean True)
            |. Parser.keyword (Parser.Token "true" ())
        , Parser.succeed (Token.Boolean False)
            |. Parser.keyword (Parser.Token "false" ())
        , Parser.succeed Token.String
            |= Util.string ()
        , Parser.succeed (Basics.negate >> Token.Number)
            |. Parser.symbol (Parser.Token "-" ())
            |= Parser.float () ()
        , Parser.succeed Token.Number
            |= Parser.float () ()
        ]


identifier : Parser Token
identifier =
    Parser.succeed Token.Identifier
        |= Parser.oneOf
            [ Parser.variable
                { expecting = ()
                , start = Char.isLower
                , inner = \c -> Char.isAlpha c || c == '\'' || c == '_'
                , reserved = Token.keywords
                }
            , Parser.variable
                { expecting = ()
                , start = Char.isUpper
                , inner = \c -> Char.isAlpha c || c == '_'
                , reserved = Set.empty
                }
            ]



--
