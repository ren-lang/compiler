module Cherry.Data.Keywords exposing 
    ( all
    , substitute, javascriptSubstitutions
    )


-- IMPORTS ---------------------------------------------------------------------


import Dict exposing (Dict)
import Set exposing (Set)


-- RESERVED WORDS --------------------------------------------------------------


{-| -}
all : Set String
all =
    Set.fromList
        [ "if", "then", "else" 
        , "import", "as", "exposing"
        , "pub", "fun", "let", "where", "and"
        ]


-- SUBSTITUTIONS ---------------------------------------------------------------


{-| -}
substitute : String -> String
substitute name =
    Dict.get name javascriptSubstitutions
        |> Maybe.withDefault name


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
