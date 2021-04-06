module Cherry.Parse.Declaration exposing 
    ( parse
    , parser
    , functionParser
    , variableParser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Declaration exposing (..)
import Cherry.Parse.Expression as Expression
import Cherry.Parse.Names as Names
import Parser exposing ((|=), (|.), Parser)


-- RUNNING THE PARSER ----------------------------------------------------------


{-| -}
parse : String -> Result (List Parser.DeadEnd) Declaration
parse input =
    Parser.run parser input


-- PARSERS ---------------------------------------------------------------------


{-| -}
parser : Parser Declaration
parser =
    Parser.oneOf
        [ functionParser
        , variableParser
        ]


-- FUNCTION PARSER -------------------------------------------------------------


{-| -}
functionParser : Parser Declaration
functionParser =
    Parser.succeed Function
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.keyword "pub"
            , Parser.succeed False
            ]
        |. Parser.spaces
        |. Parser.keyword "fun"
        |. Parser.spaces
        |= Names.variableParser
        |. Parser.spaces
        |. Parser.symbol "="
        |= Parser.sequence
            { start = " "
            , separator = ""
            , end = "=>"
            , spaces = Parser.spaces
            , item = 
                Parser.oneOf
                    [ Expression.arrayDestructureVariableParser
                    , Expression.localVariableParser
                    , Expression.objectDestructureVariableParser
                    ]
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |= Expression.parser
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.keyword "where"
                |. Parser.spaces
                |= Parser.sequence
                    { start = ""
                    , separator = "\n"
                    , end = ""
                    , spaces = Parser.chompWhile ((==) ' ')
                    , item =
                        Parser.succeed Tuple.pair
                            |= Names.variableParser
                            |. Parser.spaces
                            |. Parser.symbol "="
                            |. Parser.spaces
                            |= Expression.parser
                    , trailing = Parser.Optional
                    }
            , Parser.succeed []
            ]
        |> Parser.backtrackable


-- VARIABLE PARSER -------------------------------------------------------------


{-| -}
variableParser : Parser Declaration
variableParser =
    Parser.succeed Variable
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.keyword "pub"
            , Parser.succeed False
            ]
        |. Parser.spaces
        |. Parser.keyword "let"
        |. Parser.spaces
        |= Names.variableParser
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Expression.parser
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.keyword "where"
                |. Parser.spaces
                |= Parser.sequence
                    { start = ""
                    , separator = "\n"
                    , end = ""
                    , spaces = Parser.chompWhile ((==) ' ')
                    , item =
                        Parser.succeed Tuple.pair
                            |= Names.variableParser
                            |. Parser.spaces
                            |. Parser.symbol "="
                            |. Parser.spaces
                            |= Expression.parser
                    , trailing = Parser.Optional
                    }
            , Parser.succeed []
            ]

