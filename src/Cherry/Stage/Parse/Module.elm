module Cherry.Stage.Parse.Module exposing 
    ( run, parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Parse.Declaration as Declaration
import Cherry.Stage.Parse.Expression.Identifier as Identifier
import Parser exposing ((|=), (|.), Parser)
import Parser.Extra


-- RUNNING THE PARSER ----------------------------------------------------------


run : String -> Result (List Parser.DeadEnd) AST.Module
run input =
    Parser.run parser input


-- PARSER ----------------------------------------------------------------------


parser : Parser AST.Module
parser =
    Parser.succeed AST.Module
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.Extra.newlines
            , item = importParser
            , trailing = Parser.Optional
            }
        |. Parser.Extra.newlines
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.Extra.newlines
            , item = Declaration.parser
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |. Parser.end


-- IMPORT PARSER ---------------------------------------------------------------


importParser : Parser AST.Import
importParser =
    Parser.succeed AST.Import
        |. Parser.keyword "import"
        |. Parser.Extra.spaces
        |= Parser.Extra.string '\''
        |. Parser.Extra.spaces
        |= importNameParser
        |. Parser.Extra.spaces
        |= importExposedBindingsParser

importNameParser : Parser (List String)
importNameParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.keyword "as"
            |. Parser.Extra.spaces
            |= Parser.sequence
                { start = ""
                , separator = "."
                , end = " "
                , spaces = Parser.succeed ()
                , item = Identifier.namespaceParser
                , trailing = Parser.Forbidden
                }
        , Parser.succeed []
        ]

importExposedBindingsParser : Parser (List String)
importExposedBindingsParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.keyword "exposing"
            |. Parser.Extra.spaces
            |= Parser.sequence
                { start = "{"
                , separator = ","
                , end = "}"
                , spaces = Parser.Extra.spaces
                , item = Identifier.nameParser
                , trailing = Parser.Forbidden
                }
        , Parser.succeed []
        ]
