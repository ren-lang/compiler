module Cherry.Stage.Parse.Declaration exposing 
    ( run, parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Parse.Expression as Expression
import Cherry.Stage.Parse.Expression.Identifier as Identifier
import Cherry.Stage.Parse.Expression.Pattern as Pattern
import Parser exposing ((|=), (|.), Parser)
import Parser.Extra


-- RUNNING THE PARSER ----------------------------------------------------------


{-| -}
run : String -> Result (List Parser.DeadEnd) AST.Declaration
run input =
    Parser.run (parser |. Parser.end) input


-- PARSER ----------------------------------------------------------------------


{-| -}
parser : Parser AST.Declaration
parser =
    Parser.oneOf
        [ funParser
        , letParser
        ]


-- FUNCTION PARSER -------------------------------------------------------------


{-| -}
funParser : Parser AST.Declaration
funParser =
    Parser.succeed AST.Fun
        |= visibilityParser
        |. Parser.Extra.spaces
        |. Parser.keyword "fun"
        |. Parser.Extra.spaces
        |= Identifier.nameParser
        |. Parser.Extra.spaces
        |. Parser.symbol "="
        |. Parser.Extra.spaces
        |= Parser.loop []
            (\args ->
                Parser.oneOf
                    [ Parser.succeed (\arg -> arg :: args)
                        |= Pattern.parser
                        |. Parser.Extra.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse args)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.Extra.spaces
        |. Parser.symbol "=>"
        |. Parser.spaces
        |= Expression.parser
        |. Parser.spaces
        |= bindingsParser
        |> Parser.backtrackable


-- LET PARSER ------------------------------------------------------------------


{-| -}
letParser : Parser AST.Declaration
letParser =
    Parser.succeed AST.Let
        |= visibilityParser
        |. Parser.Extra.spaces
        |. Parser.keyword "let"
        |. Parser.Extra.spaces
        |= Identifier.nameParser
        |. Parser.Extra.spaces
        |. Parser.symbol "="
        |. Parser.Extra.spaces
        |= Expression.parser
        |. Parser.spaces
        |= bindingsParser


-- VISIBILITY PARSER -----------------------------------------------------------


{-| -}
visibilityParser : Parser AST.Visibility
visibilityParser =
    Parser.oneOf
        [ Parser.succeed AST.Public
            |. Parser.keyword "pub"
            |. Parser.Extra.spaces
        , Parser.succeed AST.Private
        ]


-- BINDINGS PARSERS ------------------------------------------------------------


{-| -}
bindingsParser : Parser (List AST.Binding)
bindingsParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.keyword "where"
            |. Parser.spaces
            |= Parser.sequence
                { start = ""
                , separator = "and"
                , end = ""
                , item = bindingParser
                , spaces = Parser.spaces
                , trailing = Parser.Forbidden
                }
        , Parser.succeed []
        ]

{-| -}
bindingParser : Parser AST.Binding
bindingParser =
    Parser.succeed AST.Binding
        |. Parser.spaces
        |= Identifier.nameParser
        |. Parser.Extra.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Expression.parser
