module Ren.Data.Declaration exposing 
    ( Declaration(..), Visibility(..), Binding(..)
    , function, variable
    , name, visibility
    , expose, conceal
    , fromJSON, decoder
    , fromSource, parser
    )


{-| ## Table of Contents

* Types
    * [Declaration](#Declaration)
    * [Visibility](#Visibility)
    * [Binding](#Binding)
    * [function](#function)
    * [variable](#variable)
* Helpers
    * Constructors
        * [function](#function)
        * [variable](#variable)
    * Getters
        * [name](#name)
        * [visibility](#visibility)
    * Visibility Helpers
        * [expose](#expose)
        * [conceal](#conceal)
* Parsing
    * [fromJSON](#fromJSON)
    * [decoder](#decoder)
    * [fromSource](#fromSource)
    * [parser](#parser)

---
## Types

@docs Declaration, Visibility, Binding

---
## Helpers

### Constructors

@docs function, variable

### Getters

@docs name, visibility

### Visibility Helpers

@docs expose, conceal

---
## Parsing

@docs fromJSON, decoder
@docs fromSource, parser


-}


-- IMPORTS ---------------------------------------------------------------------


import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))
import Parser.Extra
import Ren.Data.Expression as Expression exposing (Expression)
import Ren.Data.Expression.Pattern as Pattern
import Ren.Data.Keywords as Keywords


-- TYPES -----------------------------------------------------------------------


{-| -}
type Declaration
    = Function 
        { comment : List String
        , visibility : Visibility
        , name : String
        , args : List Expression.Pattern
        , body : Expression
        , bindings : List Binding
        }
    | Variable
        { comment : List String
        , visibility : Visibility
        , name : String
        , body : Expression
        , bindings : List Binding
        }

{-| -}
type Visibility
    = Public
    | Private

{-| -}
type Binding
    = Binding Expression.Pattern Expression


-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
function : List String -> Visibility -> String -> List Expression.Pattern -> Expression -> List Binding -> Declaration
function comment visibility_ name_ args body bindings =
    Function
        { comment = comment
        , visibility = visibility_
        , name = name_
        , args = args
        , body = body
        , bindings = bindings
        }

{-| -}
variable : List String -> Visibility -> String -> Expression -> List Binding -> Declaration
variable comment visibility_ name_ body bindings =
    Variable
        { comment = comment
        , visibility = visibility_
        , name = name_
        , body = body
        , bindings = bindings
        }



-- GETTERS ---------------------------------------------------------------------


{-| -}
name : Declaration -> String
name declaration =
    case declaration of
        Function data ->
            data.name

        Variable data ->
            data.name

{-| -}
visibility : Declaration -> Visibility
visibility declaration =
    case declaration of
        Function data ->
            data.visibility

        Variable data ->
            data.visibility


-- VISIBILITY HELPERS ----------------------------------------------------------


{-| -}
expose : Declaration -> Declaration
expose declaration =
    case declaration of
        Function data ->
            Function { data | visibility = Public }

        Variable data ->
            Variable { data | visibility = Public }

{-| -}
conceal : Declaration -> Declaration
conceal declaration =
    case declaration of
        Function data ->
            Function { data | visibility = Private }

        Variable data ->
            Variable { data | visibility = Private }


-- PARSING JSON ----------------------------------------------------------------


{-| -}
fromJSON : Json.Decode.Value -> Result Json.Decode.Error Declaration
fromJSON json =
    Json.Decode.decodeValue decoder json

{-| -}
decoder : Decoder Declaration
decoder =
    Json.Decode.oneOf
        [ functionDecoder
        , variableDecoder
        ]


-- PARSING JSON: DECLARATION.FUNCTION ------------------------------------------


{-| -}
functionDecoder : Decoder Declaration
functionDecoder =
    Json.Decode.Extra.taggedObject "Declaration.Function" <|
        Json.Decode.map6 function
            (Json.Decode.field "comment" <|
                Json.Decode.list Json.Decode.string
            )
            (Json.Decode.field "visibility" visibilityDecoder)
            (Json.Decode.field "name" Json.Decode.string)
            (Json.Decode.field "args" <|
                Json.Decode.list Pattern.decoder
            )
            (Json.Decode.field "body" Expression.decoder)
            (Json.Decode.field "bindings" <|
                Json.Decode.list bindingDecoder
            )

    
-- PARSING JSON: DECLARATION.VARIABLE ------------------------------------------


{-| -}
variableDecoder : Decoder Declaration
variableDecoder =
    Json.Decode.Extra.taggedObject "Declaration.Variable" <|
        Json.Decode.map5 variable
            (Json.Decode.field "comment" <|
                Json.Decode.list Json.Decode.string
            )
            (Json.Decode.field "visibility" visibilityDecoder)
            (Json.Decode.field "name" Json.Decode.string)
            (Json.Decode.field "body" Expression.decoder)
            (Json.Decode.field "bindings" <|
                Json.Decode.list bindingDecoder
            )


-- PARSING JSON: VISIBILITY ----------------------------------------------------


{-| -}
visibilityDecoder : Decoder Visibility
visibilityDecoder =
    Json.Decode.oneOf
        [ Json.Decode.Extra.taggedObject "Visibility.Public" <|
            Json.Decode.succeed Public
        , Json.Decode.Extra.taggedObject "Visibility.Private" <|
            Json.Decode.succeed Private
        ]


-- PARSING JSON: BINDING -------------------------------------------------------


{-| -}
bindingDecoder : Decoder Binding
bindingDecoder =
    Json.Decode.Extra.taggedObject "Binding" <|
        Json.Decode.map2 Binding
            (Json.Decode.field "name" Pattern.decoder)
            (Json.Decode.field "body" Expression.decoder)


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
fromSource : String -> Result (List Parser.DeadEnd) Declaration
fromSource source =
    Parser.run parser source

{-| -}
parser : Parser Declaration
parser =
    Parser.oneOf
        [ functionParser
        , variableParser
        ]


-- PARSING SOURCE: DECLARATION.FUNCTION ----------------------------------------


{-| -}
functionParser : Parser Declaration
functionParser =
    Parser.succeed function
        |= commentParser
        |. Parser.Extra.newlines
        |= visibilityParser
        |. Parser.Extra.spaces
        |. Parser.keyword "fun"
        |. Parser.Extra.spaces
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }
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

    
-- PARSING SOURCE: DECLARATION.VARIABLE ----------------------------------------


{-| -}
variableParser : Parser Declaration
variableParser =
    Parser.succeed variable
        |= commentParser
        |. Parser.Extra.newlines
        |= visibilityParser
        |. Parser.Extra.spaces
        |. Parser.keyword "let"
        |. Parser.Extra.spaces
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Keywords.all
            }
        |. Parser.Extra.spaces
        |. Parser.spaces
        |= Expression.parser
        |. Parser.spaces
        |= bindingsParser
        |> Parser.backtrackable


-- PARSING SOURCE: VISIBILITY --------------------------------------------------


{-| -}
visibilityParser : Parser Visibility
visibilityParser =
    Parser.oneOf
        [ Parser.succeed Public
            |. Parser.keyword "pub"
        , Parser.succeed Private
        ]


-- PARSING SOURCE: BINDING -----------------------------------------------------


{-| -}
bindingsParser : Parser (List Binding)
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
bindingParser : Parser Binding
bindingParser =
    Parser.succeed Binding
        |= Pattern.parser
        |. Parser.Extra.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Expression.parser


-- PARSING SOURCE: COMMENT -----------------------------------------------------


{-| -}
commentParser : Parser (List String)
commentParser =
    Parser.loop [] 
        (\comments ->
            Parser.oneOf
                [ Parser.succeed (\comment -> comment :: comments)
                    |= Parser.Extra.comment "//"
                    |> Parser.map Parser.Loop
                , Parser.succeed (List.reverse comments)
                    |> Parser.map Parser.Done
                ]
        )