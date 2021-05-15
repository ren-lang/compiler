module Ren.Data.Declaration exposing
    ( Declaration(..), Visibility
    , function, variable
    , name, body, visibility
    , expose, conceal
    , referencesName, referencesScopedName, referencesModule
    , fromJSON, decoder
    , fromSource, parser
    )

{-|


## Table of Contents

  - Types
      - [Declaration](#Declaration)
      - [Visibility](#Visibility)
      - [function](#function)
      - [variable](#variable)
  - Helpers
      - Constructors
          - [function](#function)
          - [variable](#variable)
      - Getters
          - [name](#name)
          - [body](#body)
          - [visibility](#visibility)
      - Visibility Helpers
          - [expose](#expose)
          - [conceal](#conceal)
      - Queries
          - [referencesName](#referencesName)
          - [referencesScopedName](#referencesScopedName)
          - [referencesModule](#referencesModule)
  - Parsing
      - [fromJSON](#fromJSON)
      - [decoder](#decoder)
      - [fromSource](#fromSource)
      - [parser](#parser)

---


## Types

@docs Declaration, Visibility

---


## Helpers


### Constructors

@docs function, variable


### Getters

@docs name, body, visibility


### Visibility Helpers

@docs expose, conceal


### Queries

@docs referencesName, referencesScopedName, referencesModule

---


## Parsing

@docs fromJSON, decoder
@docs fromSource, parser

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Ren.Data.Declaration.Visibility as Visibility
import Ren.Data.Expression as Expression exposing (Expression)
import Ren.Data.Expression.Pattern as Pattern exposing (Pattern)
import Ren.Data.Keywords as Keywords



-- TYPES -----------------------------------------------------------------------


{-| -}
type Declaration
    = Function
        { comment : List String
        , visibility : Visibility
        , name : Pattern
        , args : List Expression.Pattern
        , bindings : List Declaration
        , body : Expression
        }
    | Variable
        { comment : List String
        , visibility : Visibility
        , name : Pattern
        , bindings : List Declaration
        , body : Expression
        }


{-| -}
type alias Visibility =
    Visibility.Visibility



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
function : List String -> Visibility -> String -> List Expression.Pattern -> List Declaration -> Expression -> Declaration
function comment visibility_ name_ args bindings body_ =
    Function
        { comment = comment
        , visibility = visibility_
        , name = Pattern.Name name_
        , args = args
        , body = body_
        , bindings = bindings
        }


{-| -}
variable : List String -> Visibility -> Pattern -> List Declaration -> Expression -> Declaration
variable comment visibility_ name_ bindings body_ =
    Variable
        { comment = comment
        , visibility = visibility_
        , name = name_
        , body = body_
        , bindings = bindings
        }



-- GETTERS ---------------------------------------------------------------------


{-| -}
name : Declaration -> Pattern
name declaration =
    case declaration of
        Function data ->
            data.name

        Variable data ->
            data.name


{-| -}
body : Declaration -> Expression
body declaration =
    case declaration of
        Function data ->
            data.body

        Variable data ->
            data.body


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
            Function { data | visibility = Visibility.Public }

        Variable data ->
            Variable { data | visibility = Visibility.Public }


{-| -}
conceal : Declaration -> Declaration
conceal declaration =
    case declaration of
        Function data ->
            Function { data | visibility = Visibility.Private }

        Variable data ->
            Variable { data | visibility = Visibility.Private }



-- QUERIES ---------------------------------------------------------------------


{-| -}
referencesName : String -> Declaration -> Bool
referencesName name_ declaration =
    case declaration of
        Function data ->
            Expression.referencesName name_ data.body
                || List.any (referencesName name_) data.bindings

        Variable data ->
            Expression.referencesName name_ data.body
                || List.any (referencesName name_) data.bindings


{-| -}
referencesScopedName : List String -> String -> Declaration -> Bool
referencesScopedName namespace_ name_ declaration =
    case declaration of
        Function data ->
            Expression.referencesScopedName namespace_ name_ data.body
                || List.any (referencesScopedName namespace_ name_) data.bindings

        Variable data ->
            Expression.referencesScopedName namespace_ name_ data.body
                || List.any (referencesScopedName namespace_ name_) data.bindings


{-| -}
referencesModule : List String -> Declaration -> Bool
referencesModule namespace_ declaration =
    case declaration of
        Function data ->
            Expression.referencesModule namespace_ data.body
                || List.any (referencesModule namespace_) data.bindings

        Variable data ->
            Expression.referencesModule namespace_ data.body
                || List.any (referencesModule namespace_) data.bindings



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


{-| -}
lazyDecoder : Decoder Declaration
lazyDecoder =
    Json.Decode.lazy (\_ -> decoder)



-- PARSING JSON: DECLARATION.FUNCTION ------------------------------------------


{-| -}
functionDecoder : Decoder Declaration
functionDecoder =
    Json.Decode.Extra.taggedObject "Declaration.Function" <|
        Json.Decode.map6 function
            (Json.Decode.field "comment" <|
                Json.Decode.list Json.Decode.string
            )
            (Json.Decode.field "visibility" Visibility.decoder)
            (Json.Decode.field "name" Json.Decode.string)
            (Json.Decode.field "args" <|
                Json.Decode.list Pattern.decoder
            )
            (Json.Decode.field "bindings" <|
                Json.Decode.list lazyDecoder
            )
            (Json.Decode.field "body" Expression.decoder)



-- PARSING JSON: DECLARATION.VARIABLE ------------------------------------------


{-| -}
variableDecoder : Decoder Declaration
variableDecoder =
    Json.Decode.Extra.taggedObject "Declaration.Variable" <|
        Json.Decode.map5 variable
            (Json.Decode.field "comment" <|
                Json.Decode.list Json.Decode.string
            )
            (Json.Decode.field "visibility" Visibility.decoder)
            (Json.Decode.field "name" Pattern.decoder)
            (Json.Decode.field "bindings" <|
                Json.Decode.list lazyDecoder
            )
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


{-| -}
lazyParser : Parser Declaration
lazyParser =
    Parser.lazy (\_ -> parser)



-- PARSING SOURCE: DECLARATION.FUNCTION ----------------------------------------


{-| -}
functionParser : Parser Declaration
functionParser =
    Parser.succeed function
        |= commentParser
        |. Parser.Extra.newlines
        |= Visibility.parser
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
        |> Parser.andThen
            (\func ->
                Parser.oneOf
                    [ Parser.succeed func
                        |. Parser.symbol "{"
                        |. Parser.spaces
                        |= Parser.loop []
                            (\bindings ->
                                Parser.oneOf
                                    [ Parser.succeed (\binding -> binding :: bindings)
                                        |= lazyParser
                                        |. Parser.spaces
                                        |> Parser.map Parser.Loop
                                    , Parser.succeed (List.reverse bindings)
                                        |> Parser.map Parser.Done
                                    ]
                            )
                        |. Parser.spaces
                        |. Parser.keyword "ret"
                        |. Parser.spaces
                        |= Expression.parser
                        |. Parser.spaces
                        |. Parser.symbol "}"
                        |> Parser.backtrackable
                    , Parser.succeed func
                        |= Parser.succeed []
                        |= Expression.parser
                        |> Parser.backtrackable
                    ]
            )



-- PARSING SOURCE: DECLARATION.VARIABLE ----------------------------------------


{-| -}
variableParser : Parser Declaration
variableParser =
    Parser.succeed variable
        |= commentParser
        |. Parser.Extra.newlines
        |= Visibility.parser
        |. Parser.Extra.spaces
        |. Parser.keyword "let"
        |. Parser.Extra.spaces
        |= Pattern.parser
        |. Parser.Extra.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |> Parser.andThen
            (\var ->
                Parser.oneOf
                    [ Parser.succeed var
                        |. Parser.symbol "{"
                        |. Parser.spaces
                        |= Parser.loop []
                            (\bindings ->
                                Parser.oneOf
                                    [ Parser.succeed (\binding -> binding :: bindings)
                                        |= lazyParser
                                        |. Parser.spaces
                                        |> Parser.map Parser.Loop
                                    , Parser.succeed (List.reverse bindings)
                                        |> Parser.map Parser.Done
                                    ]
                            )
                        |. Parser.spaces
                        |. Parser.keyword "ret"
                        |. Parser.spaces
                        |= Expression.parser
                        |. Parser.spaces
                        |. Parser.symbol "}"
                        |> Parser.backtrackable
                    , Parser.succeed var
                        |= Parser.succeed []
                        |= Expression.parser
                        |> Parser.backtrackable
                    ]
            )



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
