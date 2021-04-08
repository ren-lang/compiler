module Cherry.Stage.Parse.Expression exposing 
    ( run, parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Data.Keywords as Keywords
import Cherry.Stage.Parse.Expression.Identifier as Identifier
import Cherry.Stage.Parse.Expression.Literal as Literal
import Cherry.Stage.Parse.Expression.Operator as Operator
import Cherry.Stage.Parse.Expression.Pattern as Pattern
import Parser exposing ((|=), (|.), Parser)
import Pratt


-- RUNNING THE PARSER ----------------------------------------------------------


{-| -}
run : String -> Result (List Parser.DeadEnd) AST.Expression
run input =
    Parser.run parser input


-- PARSER ----------------------------------------------------------------------


{-| -}
parser : Parser AST.Expression
parser =
    Pratt.expression
        { oneOf =
            [ parenthesisedParser
            , conditionalParser
            , applicationParser
            , accessParser
            , lambdaParser
            , Pratt.literal identifierParser
            , Pratt.literal literalParser
            ]
        , andThenOneOf =
            Operator.table
        , spaces =
            Parser.spaces
        }

{-| -}
parenthesisedParser : Pratt.Config AST.Expression -> Parser AST.Expression
parenthesisedParser prattConfig =
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= Pratt.subExpression 0 prattConfig
        |. Parser.spaces
        |. Parser.symbol ")"
        |> Parser.backtrackable



-- ACCESS PARSERS --------------------------------------------------------------


{-| -}
accessParser : Pratt.Config AST.Expression -> Parser AST.Expression
accessParser prattConfig =
    Parser.succeed (\expr accessor acessors -> AST.Access expr (accessor :: acessors))
        |= Parser.oneOf
            [ parenthesisedParser prattConfig
            , identifierParser
            , literalParser
            ]
        |= Parser.oneOf
            [ computedAccessorParser prattConfig
            , fixedAccessorParser
            ]
        |= Parser.loop []
            (\accessors ->
                Parser.oneOf
                    [ Parser.succeed (\accessor -> accessor :: accessors)
                        |= computedAccessorParser prattConfig
                        |> Parser.map Parser.Loop
                    , Parser.succeed (\accessor -> accessor :: accessors)
                        |= fixedAccessorParser
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse accessors)
                        |> Parser.map Parser.Done
                    ]
            )
        |> Parser.backtrackable

{-| -}
computedAccessorParser : Pratt.Config AST.Expression -> Parser AST.Accessor
computedAccessorParser prattConfig =
    Parser.succeed AST.Computed
        |. Parser.symbol "["
        |. Parser.spaces
        |= Pratt.subExpression 0 prattConfig
        |. Parser.spaces
        |. Parser.symbol "]"

{-| -}
fixedAccessorParser : Parser AST.Accessor
fixedAccessorParser =
    Parser.succeed AST.Fixed
        |. Parser.symbol "."
        |. Parser.spaces
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }


-- APPLICATION PARSER ----------------------------------------------------------


applicationParser : Pratt.Config AST.Expression -> Parser AST.Expression
applicationParser prattConfig =
    Parser.succeed (\f a args -> AST.Application f (a :: args))
        |= Parser.oneOf
            [ parenthesisedParser prattConfig
            , accessParser prattConfig
            , identifierParser
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ parenthesisedParser prattConfig
            , accessParser prattConfig
            , identifierParser
            , lambdaParser prattConfig
            , literalParser
            ]
        |. Parser.spaces
        |= Parser.loop [] (\args ->
            Parser.oneOf
                [ Parser.succeed (\arg -> arg :: args)
                    |= parenthesisedParser prattConfig
                    |. Parser.spaces
                    |> Parser.map Parser.Loop
                , Parser.succeed (\arg -> arg :: args)
                    |= accessParser prattConfig
                    |. Parser.spaces
                    |> Parser.map Parser.Loop
                , Parser.succeed (\arg -> arg :: args)
                    |= identifierParser
                    |. Parser.spaces
                    |> Parser.map Parser.Loop
                , Parser.succeed (\arg -> arg :: args)
                    |= lambdaParser prattConfig
                    |. Parser.spaces
                    |> Parser.map Parser.Loop
                , Parser.succeed (\arg -> arg :: args)
                    |= literalParser
                    |. Parser.spaces
                    |> Parser.map Parser.Loop
                , Parser.succeed (List.reverse args)
                    |> Parser.map Parser.Done
                ]
        )
        |> Parser.backtrackable


-- IDENTIFIER PARSERS ----------------------------------------------------------


{-| -}
identifierParser : Parser AST.Expression
identifierParser =
    Parser.succeed AST.Identifier
        |= Identifier.parser


-- CONDITIONAL PARSER ----------------------------------------------------------


{-| -}
conditionalParser : Pratt.Config AST.Expression -> Parser AST.Expression
conditionalParser prattConfig =
    Parser.succeed AST.Conditional
        |. Parser.keyword "if"
        |. Parser.spaces
        |= Pratt.subExpression 0 prattConfig
        |. Parser.keyword "then"
        |. Parser.spaces
        |= Pratt.subExpression 0 prattConfig
        |. Parser.keyword "else"
        |. Parser.spaces
        |= Pratt.subExpression 0 prattConfig


-- LAMBDA PARSER ---------------------------------------------------------------


{-| -}
lambdaParser : Pratt.Config AST.Expression -> Parser AST.Expression
lambdaParser prattConfig =
    Parser.succeed AST.Lambda
        |. Parser.keyword "fun"
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = " "
            , end = ""
            , item =
                Parser.succeed identity
                    |. Parser.spaces
                    |= Pattern.parser
            , spaces = Parser.succeed ()
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |. Parser.symbol "=>"
        |. Parser.spaces
        |= Pratt.subExpression 0 prattConfig


-- LITERAL PARSER --------------------------------------------------------------


{-| -}
literalParser : Parser AST.Expression
literalParser =
    Parser.succeed AST.Literal
        |= Literal.parser (Parser.lazy (\_ -> parser))
