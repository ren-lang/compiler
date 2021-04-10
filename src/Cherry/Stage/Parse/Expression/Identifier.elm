module Cherry.Stage.Parse.Expression.Identifier exposing 
    ( parser
    , nameParser, namespaceParser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Data.Keywords as Keywords
import Cherry.Stage.Parse.Expression.Operator as Operator
import Parser exposing ((|=), (|.), Parser)
import Set


-- PARSERS ---------------------------------------------------------------------


{-| -}
parser : Parser AST.Identifier
parser =
    Parser.oneOf
        [ localParser
        , scopedParser
        , Operator.asIdentifierParser
        , objectFieldParser
        ]

{-| -}
localParser : Parser AST.Identifier
localParser =
    Parser.succeed (AST.Local << Keywords.substitute)
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }

{-| -}
scopedParser : Parser AST.Identifier
scopedParser =
    Parser.succeed AST.Scoped
        |= Parser.sequence
            { start = ""
            , separator = "."
            , end = ""
            , item = namespaceParser
            , spaces = Parser.succeed ()
            , trailing = Parser.Mandatory
            }
        |= nameParser

{-| -}
nameParser : Parser String
nameParser =
    Parser.succeed Keywords.substitute
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }

{-| -}
namespaceParser : Parser String
namespaceParser =
    Parser.variable
        { start = Char.isUpper
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }

{-| -}
objectFieldParser : Parser AST.Identifier
objectFieldParser =
    Parser.succeed AST.ObjectField
        |. Parser.symbol "."
        |= nameParser
