module Ren.Compiler.Parse.Util exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)



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
    Parser.spaces
        |. Parser.loop ()
            (\_ ->
                Parser.oneOf
                    [ Parser.lineComment commentToken
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map Parser.Done
                    ]
            )
