module Parser.Extra exposing (..)


-- IMPORTS ---------------------------------------------------------------------


import Parser exposing ((|=), (|.), Parser)


-- PRIMITIVE PARSERS -----------------------------------------------------------


{-| -}
string : Char -> Parser String
string quoteChar =
    let
        quoteString =
            String.fromChar quoteChar

        isNotEndOrEscape c = 
            c /= quoteChar && c /= '\\'
    in
    Parser.succeed identity
        |. Parser.symbol quoteString
        |= Parser.loop []
            (\chunks ->
                Parser.oneOf
                    [ Parser.succeed (\chunk -> chunk :: chunks)
                        |. Parser.token "\\"
                        |= Parser.oneOf
                            [ Parser.succeed "\n" |. Parser.token "n"
                            , Parser.succeed "\t" |. Parser.token "t"
                            , Parser.succeed "\r" |. Parser.token "r"
                            , Parser.succeed quoteString  |. Parser.token quoteString
                            ]
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse chunks |> String.join "")
                        |. Parser.token quoteString
                        |> Parser.map Parser.Done
                    , Parser.succeed (\chunk -> chunk :: chunks)
                        |= Parser.getChompedString (Parser.chompWhile isNotEndOrEscape)
                        |> Parser.map Parser.Loop
                    ]
            )


-- WHITESPACE PARSERS ----------------------------------------------------------


spaces : Parser ()
spaces =
    Parser.chompWhile (\c -> c == ' ')

whitespace : Parser ()
whitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\t' || c == '\r' || c == '\n')

newlines : Parser ()
newlines =
    Parser.chompWhile (\c -> c == '\n' || c == '\r')
