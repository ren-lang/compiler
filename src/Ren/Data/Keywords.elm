module Ren.Data.Keywords exposing 
    ( all
    , substitute
    )


-- IMPORTS ---------------------------------------------------------------------


import Dict exposing (Dict)
import Set exposing (Set)


-- RESERVED WORDS --------------------------------------------------------------


{-| A `Set` of all the keywords reserved by Ren. So far that's:

    Set.fromList
        [ "if", "then", "else" 
        , "import", "as", "exposing"
        , "pub", "fun", "let", "where", "and"
        ]

-}
all : Set String
all =
    Set.fromList
        [ "if", "then", "else" 
        , "import", "as", "exposing"
        , "pub", "fun", "let", "where", "and"
        ]


-- SUBSTITUTIONS ---------------------------------------------------------------


{-| Most keywords in JavaScript are totally fine identifiers in Ren, we still
have to make sure none of those identifiers make their way into any compiled JS
code so this function substitutes a JS keyword with a dollar-prefixed version.

If you don't pass in a JS keyword then you just get back what you gave it.

    substitute "const" == "$const"
    substitute "foo" == "foo"

-}
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
