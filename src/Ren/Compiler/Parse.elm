module Ren.Compiler.Parse exposing
    ( moduleFromSource, moduleParser
    , declarationFromSource, declarationParser
    , expressionFromSource, expressionParser
    )

{-|

@docs moduleFromSource, moduleParser
@docs declarationFromSource, declarationParser
@docs expressionFromSource, expressionParser

-}

-- IMPORTS ---------------------------------------------------------------------

import Dict
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Pratt
import Ren.Language exposing (..)
import Set



-- PARSING MODULES -------------------------------------------------------------


{-| -}
moduleFromSource : String -> Result (List Parser.DeadEnd) Module
moduleFromSource =
    Parser.run moduleParser


{-| -}
moduleParser : Parser Module
moduleParser =
    Parser.succeed Module
        |. Parser.Extra.ignorables
        |= Parser.loop []
            (\imports_ ->
                Parser.oneOf
                    [ Parser.succeed (\i -> i :: imports_)
                        |. Parser.Extra.ignorables
                        |= importParser
                        |. Parser.Extra.ignorables
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse imports_)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.Extra.ignorables
        |= Parser.loop []
            (\declarations ->
                Parser.oneOf
                    [ Parser.succeed (\visibility declaration -> ( visibility, declaration ) :: declarations)
                        |. Parser.Extra.ignorables
                        |= visibilityParser
                        |. Parser.spaces
                        |= declarationParser
                        |. Parser.Extra.ignorables
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse declarations)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.Extra.ignorables
        |. Parser.end


{-| -}
importParser : Parser Import
importParser =
    Parser.succeed Import
        |. Parser.keyword "import"
        |. Parser.Extra.spaces
        |= Parser.oneOf
            [ Parser.Extra.string '\''
            , Parser.Extra.string '"'
            ]
        |. Parser.Extra.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.keyword "as"
                |. Parser.Extra.spaces
                |= importNamespaceParser
            , Parser.succeed []
            ]
        |. Parser.Extra.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.keyword "exposing"
                |. Parser.Extra.spaces
                |= Parser.sequence
                    { start = "{"
                    , separator = ","
                    , end = "}"
                    , item =
                        Parser.variable
                            { start = \c -> Char.isLower c || c == '#'
                            , inner = Char.isAlphaNum
                            , reserved = Ren.Language.keywords
                            }
                    , spaces = Parser.spaces
                    , trailing = Parser.Forbidden
                    }
            , Parser.succeed []
            ]


{-| -}
importNamespaceParser : Parser (List String)
importNamespaceParser =
    Parser.succeed (::)
        |= Parser.variable
            { start = Char.isUpper
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |= Parser.loop []
            (\namespaces ->
                Parser.oneOf
                    [ Parser.succeed (\ns -> ns :: namespaces)
                        |. Parser.symbol "."
                        |= Parser.variable
                            { start = Char.isUpper
                            , inner = Char.isAlphaNum
                            , reserved = Set.empty
                            }
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse namespaces)
                        |. Parser.oneOf
                            [ Parser.token " "
                            , Parser.token "\n"
                            , Parser.end
                            ]
                        |> Parser.map Parser.Done
                    ]
            )


{-| -}
visibilityParser : Parser Visibility
visibilityParser =
    Parser.oneOf
        [ Parser.succeed Public
            |. Parser.keyword "pub"
        , Parser.succeed Private
        ]



-- PARSING DECLARATIONS --------------------------------------------------------


{-| -}
declarationFromSource : String -> Result (List Parser.DeadEnd) Declaration
declarationFromSource =
    Parser.run declarationParser


{-| -}
declarationParser : Parser Declaration
declarationParser =
    Parser.oneOf
        [ functionParser
        , variableParser
        , enumParser
        ]


{-| -}
functionParser : Parser Declaration
functionParser =
    Parser.succeed Function
        |. Parser.keyword "fun"
        |. Parser.Extra.spaces
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Ren.Language.keywords
            }
        |. Parser.Extra.spaces
        |. Parser.symbol "="
        |. Parser.Extra.spaces
        |= Parser.loop []
            (\args ->
                Parser.oneOf
                    [ Parser.succeed (\arg -> arg :: args)
                        |= patternParser
                        |. Parser.Extra.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse args)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.Extra.spaces
        |. Parser.symbol "=>"
        |. Parser.Extra.ignorables
        |= expressionParser
        |> Parser.backtrackable


{-| -}
variableParser : Parser Declaration
variableParser =
    Parser.succeed Variable
        |. Parser.keyword "let"
        |. Parser.Extra.spaces
        |= patternParser
        |. Parser.Extra.spaces
        |. Parser.symbol "="
        |. Parser.Extra.ignorables
        |= expressionParser
        |> Parser.backtrackable


{-| -}
enumParser : Parser Declaration
enumParser =
    Parser.succeed Enum
        |. Parser.keyword "enum"
        |. Parser.spaces
        |= Parser.variable
            { start = Char.isUpper
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. Parser.Extra.ignorables
        |= Parser.oneOf
            [ Parser.succeed (::)
                |. Parser.symbol "="
                |. Parser.spaces
                |= variantParser
                |. Parser.Extra.ignorables
                |= Parser.loop []
                    (\variants ->
                        Parser.oneOf
                            [ Parser.succeed (\variant -> variant :: variants)
                                |. Parser.symbol "|"
                                |. Parser.spaces
                                |= variantParser
                                |. Parser.Extra.ignorables
                                |> Parser.map Parser.Loop
                            , Parser.succeed (List.reverse variants)
                                |> Parser.map Parser.Done
                            ]
                    )
            , Parser.succeed []
            ]


{-| -}
variantParser : Parser Variant
variantParser =
    Parser.succeed Variant
        |= Parser.variable
            { start = (==) '#'
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. Parser.spaces
        |= Parser.loop 0
            (\slots ->
                Parser.oneOf
                    [ Parser.succeed (slots + 1)
                        |. Parser.variable
                            { start = (==) '_'
                            , inner = Char.isAlphaNum
                            , reserved = Set.empty
                            }
                        |. Parser.Extra.ignorables
                        |> Parser.map Parser.Loop
                    , Parser.succeed slots
                        |> Parser.map Parser.Done
                    ]
            )



-- PARSING EXPRESSIONS ---------------------------------------------------------


{-| Parse an `Expression` from some Ren source code. It doesn't seem all that
likely you'll need to use this over `Declaration.fromSource` or `Module.fromSource`
but just in case it is, here you go.
-}
expressionFromSource : String -> Result (List Parser.DeadEnd) Expression
expressionFromSource =
    Parser.run expressionParser


{-| The parse used in `fromSource`. It's unclear why you'd need this, but it's
exposed just in case you do.
-}
expressionParser : Parser Expression
expressionParser =
    Parser.succeed identity
        |. Parser.Extra.ignorables
        |= Pratt.expression
            { oneOf =
                [ Pratt.literal blockParser
                , conditionalParser
                , applicationParser
                , accessParser
                , lambdaParser
                , matchParser
                , Pratt.literal literalParser
                , parenthesisedExpressionParser
                , Pratt.literal identifierParser
                ]
            , andThenOneOf =
                [ Infix Pipe |> Pratt.infixLeft 1 (Parser.symbol "|>")
                , Infix Compose |> Pratt.infixRight 9 (Parser.symbol ">>")
                , Infix Eq |> Pratt.infixLeft 4 (Parser.symbol "==")
                , Infix NotEq |> Pratt.infixLeft 4 (Parser.symbol "!=")
                , Infix Lte |> Pratt.infixLeft 4 (Parser.symbol "<=")
                , Infix Gte |> Pratt.infixLeft 4 (Parser.symbol ">=")
                , Infix And |> Pratt.infixRight 3 (Parser.symbol "&&")
                , Infix Or |> Pratt.infixRight 2 (Parser.symbol "||")
                , Infix Cons |> Pratt.infixRight 5 (Parser.symbol "::")
                , Infix Join |> Pratt.infixRight 5 (Parser.symbol "++")

                -- ONE CHARACTER OPERATORS
                , Infix Lt |> Pratt.infixLeft 4 (Parser.symbol "<")
                , Infix Gt |> Pratt.infixLeft 4 (Parser.symbol ">")
                , Infix Add |> Pratt.infixLeft 6 (Parser.symbol "+")
                , Infix Sub |> Pratt.infixLeft 6 (Parser.symbol "-")
                , Infix Mul |> Pratt.infixLeft 7 (Parser.symbol "*")
                , Infix Div |> Pratt.infixLeft 7 (Parser.symbol "/")
                , Infix Pow |> Pratt.infixRight 8 (Parser.symbol "^")
                , Infix Mod |> Pratt.infixRight 8 (Parser.symbol "%")
                ]
            , spaces = Parser.Extra.ignorables
            }


{-| -}
parenthesisedExpressionParser : Pratt.Config Expression -> Parser Expression
parenthesisedExpressionParser prattConfig =
    Parser.succeed SubExpression
        |. Parser.symbol "("
        |. Parser.Extra.ignorables
        |= Pratt.subExpression 0 prattConfig
        |. Parser.Extra.ignorables
        |. Parser.symbol ")"
        |> Parser.backtrackable


{-| -}
lazyExpressionParser : Parser Expression
lazyExpressionParser =
    Parser.lazy (\_ -> expressionParser)



-- PARSING EXPRESSIONS: ACCESS -------------------------------------------------


accessParser : Pratt.Config Expression -> Parser Expression
accessParser prattConfig =
    Parser.succeed (\expr accessor accessors -> Access expr (accessor :: accessors))
        |= Parser.oneOf
            [ literalParser
            , parenthesisedExpressionParser prattConfig
            , identifierParser
            ]
        |= accessorParser
        |= Parser.loop []
            (\accessors ->
                Parser.oneOf
                    [ Parser.succeed (\accessor -> accessor :: accessors)
                        |= accessorParser
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse accessors)
                        |> Parser.map Parser.Done
                    ]
            )
        |> Parser.backtrackable


{-| -}
accessorParser : Parser Accessor
accessorParser =
    Parser.oneOf
        [ computedAccessorParser
        , fixedAccessorParser
        ]


{-| -}
computedAccessorParser : Parser Accessor
computedAccessorParser =
    Parser.succeed Computed
        |. Parser.symbol "["
        |. Parser.Extra.ignorables
        |= lazyExpressionParser
        |. Parser.Extra.ignorables
        |. Parser.symbol "]"


{-| -}
fixedAccessorParser : Parser Accessor
fixedAccessorParser =
    Parser.succeed Fixed
        |. Parser.symbol "."
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }



-- PARSING EXPRESSIONS: APPLICATION --------------------------------------------


{-| -}
applicationParser : Pratt.Config Expression -> Parser Expression
applicationParser prattConfig =
    Parser.succeed (\function arg args -> Application function (arg :: args))
        |= Parser.oneOf
            [ accessParser prattConfig
            , parenthesisedExpressionParser prattConfig
            , identifierParser
            ]
        |. Parser.Extra.ignorables
        |= applicationArgumentParser prattConfig
        |. Parser.Extra.ignorables
        |= Parser.loop []
            (\args ->
                Parser.oneOf
                    [ Parser.succeed (\arg -> arg :: args)
                        |= applicationArgumentParser prattConfig
                        |. Parser.Extra.ignorables
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse args)
                        |> Parser.map Parser.Done
                    ]
            )
        |> Parser.backtrackable


{-| -}
applicationArgumentParser : Pratt.Config Expression -> Parser Expression
applicationArgumentParser prattConfig =
    Parser.oneOf
        [ parenthesisedExpressionParser prattConfig
        , accessParser prattConfig
        , lambdaParser prattConfig
        , literalParser
        , identifierParser
        ]



-- PARSING EXPRESSIONS: BLOCK --------------------------------------------------


{-| -}
blockParser : Parser Expression
blockParser =
    Parser.succeed Block
        |. Parser.symbol "{"
        |. Parser.Extra.ignorables
        |= Parser.loop []
            (\declarations ->
                Parser.oneOf
                    [ Parser.succeed (\declaration -> declaration :: declarations)
                        |= declarationParser
                        |. Parser.Extra.ignorables
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse declarations)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.Extra.ignorables
        |. Parser.keyword "ret"
        |. Parser.Extra.ignorables
        |= lazyExpressionParser
        |. Parser.Extra.ignorables
        |. Parser.symbol "}"
        |> Parser.backtrackable



-- PARSING EXPRESSIONS: CONDITIONAL --------------------------------------------


{-| -}
conditionalParser : Pratt.Config Expression -> Parser Expression
conditionalParser prattConfig =
    Parser.succeed Conditional
        |. Parser.keyword "if"
        |. Parser.Extra.ignorables
        |= Pratt.subExpression 0 prattConfig
        |. Parser.keyword "then"
        |. Parser.Extra.ignorables
        |= Pratt.subExpression 0 prattConfig
        |. Parser.keyword "else"
        |. Parser.Extra.ignorables
        |= Pratt.subExpression 0 prattConfig



-- PARSING EXPRESSIONS: IDENTIFIER ---------------------------------------------


{-| -}
identifierParser : Parser Expression
identifierParser =
    Parser.succeed Identifier
        |= Parser.oneOf
            [ localIdentifierParser
            , constructorIdentifierParser
            , scopedIdentifierParser
            , operatorIdentifierParser
            , fieldIdentifierParser
            ]


{-| -}
localIdentifierParser : Parser Identifier
localIdentifierParser =
    Parser.succeed Local
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Ren.Language.keywords
            }


{-| -}
constructorIdentifierParser : Parser Identifier
constructorIdentifierParser =
    Parser.succeed Constructor
        |= Parser.variable
            { start = (==) '#'
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }


{-| -}
scopedIdentifierParser : Parser Identifier
scopedIdentifierParser =
    Parser.succeed Scoped
        |= Parser.sequence
            { start = ""
            , separator = "."
            , end = ""
            , item =
                Parser.variable
                    { start = Char.isUpper
                    , inner = Char.isAlphaNum
                    , reserved = Set.empty
                    }
            , spaces = Parser.succeed ()
            , trailing = Parser.Mandatory
            }
        |= Parser.oneOf
            [ localIdentifierParser
            , constructorIdentifierParser
            ]


{-| -}
operatorIdentifierParser : Parser Identifier
operatorIdentifierParser =
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
fieldIdentifierParser : Parser Identifier
fieldIdentifierParser =
    Parser.succeed Field
        |. Parser.symbol "."
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Ren.Language.keywords
            }



-- PARSING EXPRESSIONS: LAMBDA -------------------------------------------------


{-| -}
lambdaParser : Pratt.Config Expression -> Parser Expression
lambdaParser prattConfig =
    Parser.succeed Lambda
        |. Parser.keyword "fun"
        |. Parser.Extra.ignorables
        |= Parser.sequence
            { start = ""
            , separator = " "
            , end = ""
            , item =
                Parser.succeed identity
                    |. Parser.Extra.ignorables
                    |= patternParser
            , spaces = Parser.succeed ()
            , trailing = Parser.Optional
            }
        |. Parser.Extra.ignorables
        |. Parser.symbol "=>"
        |. Parser.Extra.ignorables
        |= Pratt.subExpression 0 prattConfig



-- PARSING EXPRESSIONS: LITERAL ------------------------------------------------


{-| -}
literalParser : Parser Expression
literalParser =
    Parser.succeed Literal
        |= Parser.oneOf
            [ arrayLiteralParser
            , booleanLiteralParser
            , objectLiteralParser
            , numberLiteralParser
            , stringLiteralParser
            , templateLiteralParser
            , undefinedLiteralParser
            ]


{-| -}
arrayLiteralParser : Parser Literal
arrayLiteralParser =
    Parser.succeed Array
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , item = lazyExpressionParser
            , spaces = Parser.Extra.ignorables
            , trailing = Parser.Forbidden
            }


{-| -}
booleanLiteralParser : Parser Literal
booleanLiteralParser =
    Parser.succeed Boolean
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.keyword "true"
            , Parser.succeed False
                |. Parser.keyword "false"
            ]


{-| -}
numberLiteralParser : Parser Literal
numberLiteralParser =
    let
        numberConfig =
            { int = Just Basics.toFloat
            , hex = Just Basics.toFloat
            , octal = Just Basics.toFloat
            , binary = Just Basics.toFloat
            , float = Just identity
            }
    in
    Parser.succeed Number
        |= Parser.oneOf
            [ Parser.succeed Basics.negate
                |. Parser.symbol "-"
                |= Parser.number numberConfig
            , Parser.number numberConfig
            ]
        -- This is necessary to ensure we don't parse "123abc" as "Number 123"
        |. Parser.oneOf
            [ Parser.chompIf Char.isAlpha
                |> Parser.andThen (\_ -> Parser.problem "")
            , Parser.succeed ()
            ]
        |> Parser.backtrackable


{-| -}
objectLiteralParser : Parser Literal
objectLiteralParser =
    Parser.succeed (Dict.fromList >> Object)
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , item =
                Parser.oneOf
                    [ Parser.succeed Tuple.pair
                        |= Parser.variable
                            { start = Char.isLower
                            , inner = Char.isAlphaNum
                            , reserved = Ren.Language.keywords
                            }
                        |. Parser.Extra.ignorables
                        |. Parser.symbol ":"
                        |. Parser.Extra.ignorables
                        |= lazyExpressionParser
                        |> Parser.backtrackable
                    , Parser.succeed (\name -> ( name, Identifier (Local name) ))
                        |= Parser.variable
                            { start = Char.isLower
                            , inner = Char.isAlphaNum
                            , reserved = Ren.Language.keywords
                            }
                    ]
            , spaces = Parser.Extra.ignorables
            , trailing = Parser.Forbidden
            }


{-| -}
stringLiteralParser : Parser Literal
stringLiteralParser =
    Parser.succeed String
        |= Parser.oneOf
            [ Parser.Extra.string '"'
            , Parser.Extra.string '\''
            ]


{-| -}
templateLiteralParser : Parser Literal
templateLiteralParser =
    let
        isRawText c =
            c /= '`' && c /= '\\' && c /= '$'
    in
    Parser.succeed Template
        |. Parser.symbol "`"
        |= Parser.loop []
            (\chunks ->
                Parser.oneOf
                    [ Parser.succeed (\s -> Text s :: chunks)
                        |= Parser.Extra.stringEscape [ "`", "$" ]
                        |> Parser.map Parser.Loop
                    , Parser.succeed (\expr -> Expr expr :: chunks)
                        |. Parser.token "${"
                        |= lazyExpressionParser
                        |. Parser.token "}"
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse chunks)
                        |. Parser.token "`"
                        |> Parser.map Parser.Done
                    , Parser.getChompedString (Parser.chompWhile isRawText)
                        |> Parser.andThen
                            (\s ->
                                if s == "" then
                                    Parser.problem ""

                                else
                                    Parser.succeed (Text s :: chunks)
                            )
                        |> Parser.map Parser.Loop
                    ]
            )


{-| -}
undefinedLiteralParser : Parser Literal
undefinedLiteralParser =
    Parser.succeed Undefined
        |. Parser.symbol "()"
        |> Parser.backtrackable



-- PARSING EXPRESSIONS: MATCH --------------------------------------------------


{-| -}
matchParser : Pratt.Config Expression -> Parser Expression
matchParser prattConfig =
    Parser.succeed Match
        |. Parser.keyword "when"
        |. Parser.Extra.ignorables
        |= lazyExpressionParser
        |. Parser.Extra.ignorables
        |= Parser.loop []
            (\cases ->
                Parser.oneOf
                    [ Parser.succeed (\pattern guard expr -> ( pattern, guard, expr ) :: cases)
                        |. Parser.keyword "is"
                        |. Parser.Extra.ignorables
                        |= patternParser
                        |. Parser.Extra.ignorables
                        |= Parser.oneOf
                            [ Parser.succeed Just
                                |. Parser.keyword "if"
                                |. Parser.Extra.ignorables
                                |= Pratt.subExpression 0 prattConfig
                                |. Parser.Extra.ignorables
                            , Parser.succeed Nothing
                            ]
                        |. Parser.symbol "=>"
                        |. Parser.Extra.ignorables
                        |= Pratt.subExpression 0 prattConfig
                        |> Parser.map Parser.Loop
                    , Parser.succeed (\expr -> ( Wildcard Nothing, Nothing, expr ) :: cases)
                        |. Parser.keyword "else"
                        |. Parser.Extra.ignorables
                        |. Parser.symbol "=>"
                        |. Parser.Extra.ignorables
                        |= Pratt.subExpression 0 prattConfig
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse cases)
                        |> Parser.map Parser.Done
                    ]
            )



-- PARSING PATTERNS ------------------------------------------------------------


{-| -}
patternParser : Parser Pattern
patternParser =
    let
        patternParsers =
            [ arrayDestructureParser
            , nameParser
            , objectDestructureParser
            , valueParser
            , variantDestructureParser
            , wildcardParser
            ]
    in
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "("
            |. Parser.spaces
            |= Parser.oneOf patternParsers
            |. Parser.spaces
            |. Parser.symbol ")"
            |> Parser.backtrackable
        , Parser.oneOf patternParsers
        ]


{-| -}
lazyPatternParser : Parser Pattern
lazyPatternParser =
    Parser.lazy (\_ -> patternParser)


{-| -}
arrayDestructureParser : Parser Pattern
arrayDestructureParser =
    Parser.succeed ArrayDestructure
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item = lazyPatternParser
            , trailing = Parser.Forbidden
            }


{-| -}
nameParser : Parser Pattern
nameParser =
    Parser.succeed Name
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Ren.Language.keywords
            }


{-| -}
objectDestructureParser : Parser Pattern
objectDestructureParser =
    Parser.succeed ObjectDestructure
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item =
                Parser.succeed Tuple.pair
                    |= Parser.variable
                        { start = Char.isLower
                        , inner = Char.isAlphaNum
                        , reserved = Ren.Language.keywords
                        }
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.succeed Just
                            |. Parser.symbol ":"
                            |. Parser.spaces
                            |= lazyPatternParser
                        , Parser.succeed Nothing
                        ]
            , trailing = Parser.Forbidden
            }


{-| -}
valueParser : Parser Pattern
valueParser =
    Parser.succeed Value
        |= Parser.oneOf
            [ booleanLiteralParser
            , numberLiteralParser
            , stringLiteralParser
            ]


{-| -}
variantDestructureParser : Parser Pattern
variantDestructureParser =
    Parser.succeed VariantDestructure
        |= Parser.variable
            { start = (==) '#'
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. Parser.spaces
        |= Parser.loop []
            (\patterns ->
                Parser.oneOf
                    [ Parser.succeed (\pattern -> pattern :: patterns)
                        |= patternParser
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse patterns)
                        |> Parser.map Parser.Done
                    ]
            )


{-| -}
wildcardParser : Parser Pattern
wildcardParser =
    Parser.succeed Wildcard
        |. Parser.symbol "_"
        |= Parser.oneOf
            [ Parser.succeed Just
                |= Parser.variable
                    { start = Char.isLower
                    , inner = Char.isAlphaNum
                    , reserved = Ren.Language.keywords
                    }
            , Parser.succeed Nothing
            ]
