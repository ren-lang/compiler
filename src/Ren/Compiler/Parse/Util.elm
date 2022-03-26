module Ren.Compiler.Parse.Util exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)



-- COMBINATORS -----------------------------------------------------------------


{-| -}
many : Parser c x a -> Parser c x (List a)
many p =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> x :: xs)
                    |= p
                    |> Parser.map Parser.Loop
                , Parser.succeed ()
                    |> Parser.map (\_ -> List.reverse xs)
                    |> Parser.map Parser.Done
                ]
        )


{-| -}
oneOrMoreOf : x -> Parser c x a -> Parser c x (List a)
oneOrMoreOf x p =
    many p
        |> Parser.andThen
            (\xs ->
                if List.isEmpty xs then
                    Parser.problem x

                else
                    Parser.succeed xs
            )



-- WHITESPACE PARSERS ----------------------------------------------------------


{-| -}
spaces : Parser c x ()
spaces =
    Parser.chompWhile (\c -> c == ' ')


{-| -}
whitespace : Parser c x ()
whitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')


{-| -}
newline : x -> Parser c x ()
newline x =
    Parser.oneOf
        [ Parser.token <| Parser.Token "\u{000D}\n" x
        , Parser.token <| Parser.Token "\n" x
        ]


{-| -}
newlines : Parser c x ()
newlines =
    Parser.chompWhile (\c -> c == '\n' || c == '\u{000D}')



-- COMMENTS --------------------------------------------------------------------


{-| -}
comment : Parser.Token x -> Parser c x String
comment commentToken =
    Parser.lineComment commentToken
        |> Parser.getChompedString



-- IGNORABLES ------------------------------------------------------------------


{-| -}
ignorables : Parser.Token x -> Parser c x ()
ignorables commentToken =
    whitespace |. many (whitespace |. Parser.lineComment commentToken |. whitespace)



-- STRINGS ---------------------------------------------------------------------


string : x -> Parser c x String
string x =
    Parser.succeed Basics.identity
        |. Parser.token (Parser.Token "\"" x)
        |= Parser.loop [] (stringHelp x)


stringHelp : x -> List String -> Parser c x (Parser.Step (List String) String)
stringHelp x revChunks =
    Parser.oneOf
        [ Parser.succeed (\chunk -> Parser.Loop (chunk :: revChunks))
            |. Parser.token (Parser.Token "\\" x)
            |= Parser.oneOf
                [ Parser.map (\_ -> "\n") (Parser.token (Parser.Token "n" x))
                , Parser.map (\_ -> "\t") (Parser.token (Parser.Token "t" x))
                , Parser.map (\_ -> "\u{000D}") (Parser.token (Parser.Token "r" x))
                , Parser.succeed String.fromChar
                    |. Parser.token (Parser.Token "u{" x)
                    |= unicode (Basics.always x)
                    |. Parser.token (Parser.Token "}" x)
                , Parser.map (\_ -> "\"") (Parser.token (Parser.Token "\"" x))
                ]
        , Parser.token (Parser.Token "\"" x)
            |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse revChunks)))
        , Parser.chompWhile isUninteresting
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'



-- UNICODE


unicode : (String -> x) -> Parser c x Char
unicode x =
    Parser.getChompedString (Parser.chompWhile Char.isHexDigit)
        |> Parser.andThen (codeToChar x)


codeToChar : (String -> x) -> String -> Parser c x Char
codeToChar x str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        Parser.problem <| x "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        Parser.succeed (Char.fromCode code)

    else
        Parser.problem <| x "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)
