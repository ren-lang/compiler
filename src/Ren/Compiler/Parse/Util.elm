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
