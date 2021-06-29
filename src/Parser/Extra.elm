module Parser.Extra exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Parser exposing ((|.), (|=), Parser)



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
                            , Parser.succeed "\u{000D}" |. Parser.token "r"
                            , Parser.succeed quoteString |. Parser.token quoteString
                            ]
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse chunks |> String.join "")
                        |. Parser.token quoteString
                        |> Parser.map Parser.Done
                    , Parser.getChompedString (Parser.chompWhile isNotEndOrEscape)
                        |> Parser.andThen
                            (\s ->
                                if s == "" then
                                    Parser.problem ""

                                else
                                    Parser.succeed (s :: chunks)
                            )
                        |> Parser.map Parser.Loop
                    ]
            )



-- WHITESPACE PARSERS ----------------------------------------------------------


spaces : Parser ()
spaces =
    Parser.chompWhile (\c -> c == ' ')


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\t' || c == '\u{000D}' || c == '\n')


newline : Parser ()
newline =
    Parser.oneOf
        [ Parser.token "\u{000D}\n"
        , Parser.token "\n"
        ]


newlines : Parser ()
newlines =
    Parser.chompWhile (\c -> c == '\n' || c == '\u{000D}')



-- COMMENTS --------------------------------------------------------------------


{-| -}
comment : String -> Parser String
comment start =
    Parser.lineComment start
        |> Parser.getChompedString



-- IGNORABLES ------------------------------------------------------------------


ignorables : Parser ()
ignorables =
    Parser.oneOf
        [ Parser.spaces
            |. Parser.lineComment "//"
            |> Parser.backtrackable
        , Parser.spaces
        ]



-- DEADENDS --------------------------------------------------------------------


{-| -}
deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    List.foldl (++) "" (List.map deadEndToString deadEnds)


{-| -}
deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
    let
        position =
            "row:" ++ String.fromInt deadEnd.row ++ " col:" ++ String.fromInt deadEnd.col ++ "\n"
    in
    case deadEnd.problem of
        Parser.Expecting str ->
            "Expecting " ++ str ++ " at " ++ position

        Parser.ExpectingInt ->
            "ExpectingInt at " ++ position

        Parser.ExpectingHex ->
            "ExpectingHex at " ++ position

        Parser.ExpectingOctal ->
            "ExpectingOctal at " ++ position

        Parser.ExpectingBinary ->
            "ExpectingBinary at " ++ position

        Parser.ExpectingFloat ->
            "ExpectingFloat at " ++ position

        Parser.ExpectingNumber ->
            "ExpectingNumber at " ++ position

        Parser.ExpectingVariable ->
            "ExpectingVariable at " ++ position

        Parser.ExpectingSymbol str ->
            "ExpectingSymbol " ++ str ++ " at " ++ position

        Parser.ExpectingKeyword str ->
            "ExpectingKeyword " ++ str ++ "at " ++ position

        Parser.ExpectingEnd ->
            "ExpectingEnd at " ++ position

        Parser.UnexpectedChar ->
            "UnexpectedChar at " ++ position

        Parser.Problem str ->
            "ProblemString " ++ str ++ " at " ++ position

        Parser.BadRepeat ->
            "BadRepeat at " ++ position
