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