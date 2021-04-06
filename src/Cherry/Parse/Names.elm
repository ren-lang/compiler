module Cherry.Parse.Names exposing 
    ( variableParser
    , namespaceParser
    , scopedParser
    , moduleParser
    , keywords, javascriptSubstitutions
    )


-- IMPORTS ---------------------------------------------------------------------


import Dict exposing (Dict)
import Parser exposing ((|=), (|.), Parser)
import Set exposing (Set)


-- PARSERS ---------------------------------------------------------------------


{-| -}
variableParser : Parser String
variableParser =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = keywords
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
scopedParser : Parser ( List String, String )
scopedParser =
    Parser.succeed Tuple.pair
        |= Parser.sequence
            { start = ""
            , separator = "."
            , end = ""
            , spaces = Parser.succeed ()
            , item = namespaceParser
            , trailing = Parser.Mandatory
            }
        |= variableParser

{-| -}
moduleParser : Parser (List String)
moduleParser =
    Parser.sequence
        { start = ""
        , separator = "."
        , end = ""
        , spaces = Parser.succeed ()
        , item = namespaceParser
        , trailing = Parser.Forbidden
        }


-- RESERVED WORDS --------------------------------------------------------------


{-| -}
keywords : Set String
keywords =
    Set.fromList <|
        Dict.keys javascriptSubstitutions ++
            -- This is all the keywords in Cherry that are invalid identifiers.
            -- There's some overlap with the JavaScript keywords but it's easier
            -- to define duplicates vs remembering which words are keywords in
            -- js.
            [ "if"
            , "then"
            , "else" 
            , "fun"
            , "pub"
            , "let"
            , "where"
            ]

{-| -}
javascriptSubstitutions : Dict String String
javascriptSubstitutions =
    Dict.fromList
        [ ( "await", "$await" ) 
        , ( "break", "$break" ) 
        , ( "case", "$case" ) 
        , ( "catch", "$catch" ) 
        , ( "class", "$class" ) 
        , ( "const", "$const" ) 
        , ( "continue", "$continue" ) 
        , ( "debugger", "$debugger" ) 
        , ( "default", "$default" ) 
        , ( "delete", "$delete" ) 
        , ( "do", "$do" ) 
        , ( "else", "$else" ) 
        , ( "enum", "$enum" ) 
        , ( "export", "$export" ) 
        , ( "extends", "$extends" ) 
        , ( "false", "$false" ) 
        , ( "finally", "$finally" ) 
        , ( "for", "$for" ) 
        , ( "function", "$function" ) 
        , ( "if", "$if" ) 
        , ( "implements", "$implements" ) 
        , ( "import", "$import" ) 
        , ( "in", "$in" ) 
        , ( "instanceof", "$instanceof" ) 
        , ( "interface", "$interface" ) 
        , ( "let", "$let" ) 
        , ( "new", "$new" ) 
        , ( "null", "$null" ) 
        , ( "package", "$package" ) 
        , ( "private", "$private" ) 
        , ( "protected", "$protected" ) 
        , ( "public", "$public" ) 
        , ( "return", "$return" ) 
        , ( "super", "$super" ) 
        , ( "switch", "$switch" ) 
        , ( "static", "$static" ) 
        , ( "this", "$this" ) 
        , ( "throw", "$throw" ) 
        , ( "try", "$try" ) 
        , ( "true", "$true" ) 
        , ( "typeof", "$typeof" ) 
        , ( "var", "$var" ) 
        , ( "void", "$void" ) 
        , ( "while", "$while" ) 
        , ( "with", "$with" ) 
        , ( "yield", "$yield" ) 
        ]