module Cherry.Parse.Expression exposing 
    ( parse
    , parser, parenthesisedExpressionParser
    , accessParser
    , fixedAccessorParser, computedAccessorParser
    , conditionalParser
    , applicationParser
    , lambdaParser
    , literalParser
    , numberLiteralParser, stringLiteralParser, booleanParser, arrayLiteralParser, objectLiteralParser
    , variableParser
    , arrayDestructureVariableParser, objectDestructureVariableParser, localVariableParser, scopedVariableParser, operatorVariableParser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Expression exposing (..)
import Cherry.Parse.Names as Names
import Parser exposing ((|=), (|.), Parser)
import Pratt


-- RUNNING THE PARSER ----------------------------------------------------------


{-| -}
parse : String -> Result (List Parser.DeadEnd) Expression
parse input =
    Parser.run parser input


-- PARSERS ---------------------------------------------------------------------


{-| -}
parser : Parser Expression
parser =
    Pratt.expression
        { oneOf =
            [ applicationParser
            , accessParser
            , parenthesisedExpressionParser
            , conditionalParser
            , lambdaParser
            , literalParser
            , Pratt.literal variableParser
            ]
        , andThenOneOf =
            -- TWO CHARACTER OPERATORS
            [ InfixOp Pipe    |> Pratt.infixLeft 1  (Parser.symbol "|>")
            , InfixOp Compose |> Pratt.infixRight 9 (Parser.symbol ">>")
            , InfixOp Eq      |> Pratt.infixLeft 4  (Parser.symbol "==")
            , InfixOp NotEq   |> Pratt.infixLeft 4  (Parser.symbol "!=")
            , InfixOp Lte     |> Pratt.infixLeft 4  (Parser.symbol "<=")
            , InfixOp Gte     |> Pratt.infixLeft 4  (Parser.symbol ">=")
            , InfixOp And     |> Pratt.infixRight 3 (Parser.symbol "&&")
            , InfixOp Or      |> Pratt.infixRight 2 (Parser.symbol "||")
            , InfixOp Cons    |> Pratt.infixRight 5 (Parser.symbol "::")
            , InfixOp Join    |> Pratt.infixRight 5 (Parser.symbol "++")

            -- ONE CHARACTER OPERATORS
            , InfixOp Discard |> Pratt.infixRight 1 (Parser.symbol ";")
            , InfixOp Lt      |> Pratt.infixLeft 4  (Parser.symbol "<")
            , InfixOp Gt      |> Pratt.infixLeft 4  (Parser.symbol ">")
            , InfixOp Add     |> Pratt.infixLeft 6  (Parser.symbol "+")
            , InfixOp Sub     |> Pratt.infixLeft 6  (Parser.symbol "-")
            , InfixOp Mul     |> Pratt.infixLeft 7  (Parser.symbol "*")
            , InfixOp Div     |> Pratt.infixLeft 7  (Parser.symbol "/")
            , InfixOp Pow     |> Pratt.infixRight 8 (Parser.symbol "^")
            , InfixOp Mod     |> Pratt.infixRight 8 (Parser.symbol "%")
            ]
        , spaces = Parser.spaces
        }

{-| -}
parenthesisedExpressionParser : Pratt.Config Expression -> Parser Expression
parenthesisedExpressionParser config =
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.spaces
        |. Parser.symbol ")"
        |> Parser.backtrackable


-- ACCESSOR PARSERS ------------------------------------------------------------


{-| -}
accessParser : Pratt.Config Expression -> Parser Expression
accessParser config =
    Parser.succeed Access
        |= Parser.oneOf
            [ parenthesisedExpressionParser config
            , literalParser config
            , variableParser
            ]
        |= Parser.oneOf
            [ fixedAccessorParser
            , computedAccessorParser config
            ]
        |> Parser.backtrackable

{-| -}
fixedAccessorParser : Parser Accessor
fixedAccessorParser =
    Parser.succeed Fixed
        |. Parser.symbol "."
        |= Names.variableParser

{-| -}
computedAccessorParser : Pratt.Config Expression -> Parser Accessor
computedAccessorParser config =
    Parser.succeed Computed
        |. Parser.symbol "["
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.spaces
        |. Parser.symbol "]"



-- CONDITIONAL PARSER ----------------------------------------------------------


{-| -}
conditionalParser : Pratt.Config Expression -> Parser Expression
conditionalParser config =
    Parser.succeed Conditional
        |. Parser.keyword "if"
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.keyword "then"
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.keyword "else"
        |. Parser.spaces
        |= Pratt.subExpression 0 config


-- FUNCTION APPLICATION PARSER -------------------------------------------------


{-| -}
applicationParser : Pratt.Config Expression -> Parser Expression
applicationParser config =
    Parser.succeed Application
        |= Parser.oneOf
            [ accessParser config
            , parenthesisedExpressionParser config
            , literalParser config
            , variableParser
            ]
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |> Parser.backtrackable
    

-- LAMBDA PARSER ---------------------------------------------------------------


{-| -}
lambdaParser : Pratt.Config Expression -> Parser Expression
lambdaParser config =
    Parser.succeed Lambda
        |. Parser.keyword "fun"
        |= Parser.sequence
            { start = " "
            , separator = ""
            , end = "=>"
            , spaces = Parser.spaces
            , item = 
                Parser.oneOf
                    [ arrayDestructureVariableParser
                    , localVariableParser
                    , objectDestructureVariableParser
                    ]
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |= Pratt.subExpression 0 config


-- LITERAL PARSERS -------------------------------------------------------------


{-| -}
literalParser : Pratt.Config Expression -> Parser Expression
literalParser config =
    Parser.map Literal <|
        Parser.oneOf
            [ numberLiteralParser
            , stringLiteralParser
            , booleanParser
            , arrayLiteralParser config
            , objectLiteralParser config
            ]

{-| -}
numberLiteralParser : Parser Literal
numberLiteralParser =
    Parser.succeed Number
        |= Parser.float
        |> Parser.backtrackable

{-| -}
stringLiteralParser : Parser Literal
stringLiteralParser =
    let
        isNotEndOrEscape c = 
            c /= '\'' && c /= '\\'
    in
    Parser.succeed String
        |. Parser.symbol "'"
        |= Parser.loop []
            (\chunks ->
                Parser.oneOf
                    [ Parser.succeed (\chunk -> chunk :: chunks)
                        |. Parser.token "\\"
                        |= Parser.oneOf
                            [ Parser.succeed "\n" |. Parser.token "n"
                            , Parser.succeed "\t" |. Parser.token "t"
                            , Parser.succeed "\r" |. Parser.token "r"
                            , Parser.succeed "'"  |. Parser.token "'"
                            ]
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse chunks |> String.join "")
                        |. Parser.token "'"
                        |> Parser.map Parser.Done
                    , Parser.succeed (\chunk -> chunk :: chunks)
                        |= Parser.getChompedString (Parser.chompWhile isNotEndOrEscape)
                        |> Parser.map Parser.Loop
                    ]
            )

{-| -}
booleanParser : Parser Literal
booleanParser =
    Parser.succeed Boolean
        |= Parser.oneOf
            [ Parser.succeed True |. Parser.keyword "true"
            , Parser.succeed False |. Parser.keyword "false"
            ]

{-| -}
arrayLiteralParser : Pratt.Config Expression -> Parser Literal
arrayLiteralParser config =
    Parser.succeed Array
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item = Pratt.subExpression 0 config
            , trailing = Parser.Forbidden
            }

{-| -}
objectLiteralParser : Pratt.Config Expression -> Parser Literal
objectLiteralParser config =
    Parser.succeed Object
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item =
                Parser.succeed Tuple.pair
                    |= Names.variableParser
                    |. Parser.spaces
                    |. Parser.symbol ":"
                    |. Parser.spaces
                    |= Pratt.subExpression 0 config
            , trailing = Parser.Forbidden
            }


-- VARIABLE PARSERS ------------------------------------------------------------


{-| -}
variableParser : Parser Expression
variableParser =
    Parser.map Variable <|
        Parser.oneOf
            [ arrayDestructureVariableParser
            , localVariableParser
            , objectDestructureVariableParser
            , operatorVariableParser
            , scopedVariableParser
            ]

{-| -}
arrayDestructureVariableParser : Parser Variable
arrayDestructureVariableParser =
    Parser.succeed ArrayDestructure
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item =
                Parser.oneOf
                    [ Parser.lazy (\_ -> arrayDestructureVariableParser)
                    , localVariableParser
                    , Parser.lazy (\_ -> objectDestructureVariableParser)
                    ]
            , trailing = Parser.Forbidden
            }

{-| -}
localVariableParser : Parser Variable
localVariableParser =
    Parser.map Local Names.variableParser

{-| -}
objectDestructureVariableParser : Parser Variable
objectDestructureVariableParser =
    Parser.succeed ObjectDestructure
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item =
                Parser.oneOf
                    [ Parser.lazy (\_ -> arrayDestructureVariableParser)
                    , localVariableParser
                    , Parser.lazy (\_ -> objectDestructureVariableParser)
                    ]
            , trailing = Parser.Forbidden
            }

{-| -}
operatorVariableParser : Parser Variable
operatorVariableParser =
    Parser.succeed Operator
        |. Parser.symbol "("
        |= Parser.oneOf
            [ Parser.succeed Pipe |. Parser.symbol "|>"
            , Parser.succeed Compose |. Parser.symbol ">>"
            , Parser.succeed Eq |. Parser.symbol "=="
            , Parser.succeed NotEq |. Parser.symbol "!="
            , Parser.succeed Lte |. Parser.symbol "<="
            , Parser.succeed Gte |. Parser.symbol ">="
            , Parser.succeed And |. Parser.symbol "&&"
            , Parser.succeed Or |. Parser.symbol "||"
            , Parser.succeed Cons |. Parser.symbol "::"
            , Parser.succeed Join |. Parser.symbol "++"
            , Parser.succeed Lt |. Parser.symbol "<"
            , Parser.succeed Gt |. Parser.symbol ">"
            , Parser.succeed Add |. Parser.symbol "+"
            , Parser.succeed Sub |. Parser.symbol "-"
            , Parser.succeed Mul |. Parser.symbol "*"
            , Parser.succeed Div |. Parser.symbol "/"
            , Parser.succeed Pow |. Parser.symbol "^"
            , Parser.succeed Mod |. Parser.symbol "%"
            ]
        |. Parser.symbol ")"

{-| -}
scopedVariableParser : Parser Variable
scopedVariableParser =
    Parser.map (\( namespaces, name ) -> Scoped namespaces name )
        Names.scopedParser
