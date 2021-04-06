module Cherry.Parse.Module exposing 
    ( parse
    , parser
    , importParser, pathParser, aliasParser, exposingParser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Module exposing (..)
import Cherry.Parse.Declaration as Declaration
import Cherry.Parse.Names as Names
import Parser exposing ((|=), (|.), Parser)


-- RUNNING THE PARSER ----------------------------------------------------------


{-| -}
parse : String -> Result (List Parser.DeadEnd) Module
parse input =
    Parser.run parser input


-- PARSERS ---------------------------------------------------------------------


{-| -}
parser : Parser Module
parser =
    Parser.succeed Module
        |= Parser.sequence
            { start = ""
            , separator = "\n"
            , end = ""
            , spaces = Parser.succeed ()
            , item = importParser
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item = Declaration.parser
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |. Parser.end


-- IMPORT PARSER ---------------------------------------------------------------


{-| -}
importParser : Parser Import
importParser =
    Parser.succeed Import
        |. Parser.keyword "import"
        |. Parser.spaces
        |= pathParser
        |. Parser.spaces
        |= aliasParser
        |. Parser.spaces
        |= exposingParser

{-| -}
pathParser : Parser String
pathParser =
    Parser.succeed (String.join "/")
        |= Parser.sequence
            { start = "'"
            , separator = "/"
            , end = "'"
            , spaces = Parser.succeed ()
            , item = 
                Parser.chompWhile (\c -> c /= '\'' && c/= '/')
                    |> Parser.getChompedString
            , trailing = Parser.Forbidden
            }

{-| -}
aliasParser : Parser (Maybe (List String))
aliasParser =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.keyword "as"
            |. Parser.spaces
            |= Names.moduleParser
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]

{-| -}
exposingParser : Parser (Maybe (List String))
exposingParser =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.keyword "exposing"
            |. Parser.spaces
            |= Parser.sequence
                { start = "{"
                , separator = ","
                , end = "}"
                , spaces = Parser.spaces
                , item = Names.variableParser
                , trailing = Parser.Forbidden
                }
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]

