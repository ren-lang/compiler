module Cherry.Stage.Parse.Expression.Literal exposing 
    ( parser
    , containerParser, primitiveParser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Parse.Expression.Identifier as Identifier
import Parser exposing ((|=), (|.), Parser)
import Parser.Extra


-- PARSERS ---------------------------------------------------------------------


{-| -}
parser : Parser AST.Expression -> Parser AST.Literal
parser expressionParser =
    Parser.oneOf
        [ containerParser expressionParser
        , primitiveParser
        ]

{-| -}
containerParser : Parser AST.Expression -> Parser AST.Literal
containerParser expressionParser =
    Parser.oneOf
        [ arrayParser expressionParser
        , objectParser expressionParser
        ]

{-| -}
primitiveParser : Parser AST.Literal
primitiveParser =
    Parser.oneOf
        [ booleanParser
        , numberParser
        , stringParser
        ]


-- ARRAY PARSER ----------------------------------------------------------------


{-| -}
arrayParser : Parser AST.Expression -> Parser AST.Literal
arrayParser expressionParser =
    Parser.succeed AST.Array
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , item = expressionParser
            , spaces = Parser.spaces
            , trailing = Parser.Forbidden
            }


-- BOOLEAN PARSER --------------------------------------------------------------


{-| -}
booleanParser : Parser AST.Literal
booleanParser =
    Parser.succeed AST.Boolean
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.keyword "true"
            , Parser.succeed False
                |. Parser.keyword "false"
            ]


-- NUMBER PARSER ---------------------------------------------------------------


{-| -}
numberParser : Parser AST.Literal
numberParser =
    Parser.succeed AST.Number
        |= Parser.number
            { int = Just Basics.toFloat
            , hex = Just Basics.toFloat
            , octal = Just Basics.toFloat
            , binary = Just Basics.toFloat
            , float = Just identity
            }
    in
    Parser.succeed AST.Number
        |= Parser.oneOf
            [ Parser.succeed Basics.negate
                |. Parser.symbol "-" 
                |= Parser.number numberConfig
            , Parser.number numberConfig
            ]
        -- This is necessary to ensure we don't parse "123abc" as "AST.Number 123"
        |. Parser.oneOf
            [ Parser.chompIf (Char.isAlpha)
                |> Parser.andThen (\_ -> Parser.problem "")
            , Parser.succeed ()
            ]


-- OBJECT PARSER ---------------------------------------------------------------


{-| -}
objectParser : Parser AST.Expression -> Parser AST.Literal
objectParser expressionParser =
    Parser.succeed AST.Object
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , item = 
                Parser.oneOf
                    [ Parser.succeed Tuple.pair
                        |= Identifier.nameParser
                        |. Parser.spaces
                        |. Parser.symbol ":"
                        |. Parser.spaces
                        |= expressionParser
                        |> Parser.backtrackable
                    , Parser.succeed (\name -> ( name , AST.Identifier (AST.Local name) ))
                        |= Identifier.nameParser
                    ]
            , spaces = Parser.spaces
            , trailing = Parser.Forbidden
            }


-- STRING PARSER ---------------------------------------------------------------


{-| -}
stringParser : Parser AST.Literal
stringParser =
    Parser.succeed AST.String
        |= Parser.oneOf
            [ Parser.Extra.string '"'
            , Parser.Extra.string '\''
            ]
