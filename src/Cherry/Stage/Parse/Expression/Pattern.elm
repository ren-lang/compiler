module Cherry.Stage.Parse.Expression.Pattern exposing (..)


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Parse.Expression.Identifier as Identifier
import Cherry.Stage.Parse.Expression.Literal as Literal
import Parser exposing ((|=), (|.), Parser)


-- PARSERS ---------------------------------------------------------------------


parser : Parser AST.Pattern
parser =
    Parser.oneOf
        [ arrayDestructureParser
        , nameParser
        , objectDestructureParser
        , valueParser
        , wildcardParser
        ]

arrayDestructureParser : Parser AST.Pattern
arrayDestructureParser =
    Parser.succeed AST.ArrayDestructure
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item = Parser.lazy (\_ -> parser)
            , trailing = Parser.Forbidden
            }

nameParser : Parser AST.Pattern
nameParser =
    Parser.succeed AST.Name
        |= Identifier.nameParser

objectDestructureParser : Parser AST.Pattern
objectDestructureParser =
    Parser.succeed AST.ObjectDestructure
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item =
                Parser.succeed Tuple.pair
                    |= Identifier.nameParser
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.succeed Just
                            |. Parser.symbol ":"
                            |. Parser.spaces
                            |= Parser.lazy (\_ -> parser)
                        , Parser.succeed Nothing
                        ]
            , trailing = Parser.Forbidden
            }

valueParser : Parser AST.Pattern
valueParser =
    Parser.succeed AST.Value
        |= Literal.primitiveParser

{-| -}
wildcardParser : Parser AST.Pattern
wildcardParser =
    Parser.succeed AST.Wildcard
        |. Parser.symbol "_"
        |= Parser.oneOf
            [ Parser.succeed Just
                |= Identifier.nameParser
            , Parser.succeed Nothing
            ]
