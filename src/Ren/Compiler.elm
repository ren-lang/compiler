module Ren.Compiler exposing
    ( compile, compileTo
    , Module, Declaration, Expression
    , parse, parseDeclaration, parseExpression
    , decode, decodeDeclaration, decodeExpression
    , optimise, optimiseDeclaration, optimiseExpression
    , Target(..), emit, emitDeclaration, emitExpression
    )

{-|


## Table of Contents

  - Running The Compiler
      - [compile](#compile)
      - [compileTo](#compileTo)
  - Types
      - [Module](#Module)
      - [Declaration](#Declaration)
      - [Expression](#Expression)
  - Parsing
      - [parse](#parse)
      - [parseDeclaration](#parseDeclaration)
      - [parseExpression](#parseExpression)
      - [decode](#decode)
      - [decodeDeclaration](#decodeDeclaration)
      - [decodeExpression](#decodeExpression)
  - Optimisation
      - [optimise](#optimise)
      - [optimiseDeclaration](#optimiseDeclaration)
      - [optimiseExpression](#optimiseExpression)
  - Emitting Target Code
      - [Target](#Target)
      - [emit](#emit)
      - [emitDeclaration](#emitDeclaration)
      - [emitExpression](#emitExpression)

---


## Running The Compiler

@docs compile, compileTo

---


## Types

This module aliases the main types that you might be interested in working with.
See the appropriate modules (e.g [`Ren.Data.Expression`](./Ren-Data-Expression))
for more documentation about that type and the things you might want to do with
it.

The alises are provided here as a convenience so you only need to import
`Ren.Compiler` if you don't want to do anything fancy!

@docs Module, Declaration, Expression

---


## Parsing

@docs parse, parseDeclaration, parseExpression
@docs decode, decodeDeclaration, decodeExpression

---


## Optimisation

@docs optimise, optimiseDeclaration, optimiseExpression

---


## Emitting Target Code

@docs Target, emit, emitDeclaration, emitExpression

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Parser
import Ren.Compiler.Emit.CommonJS as CommonJS
import Ren.Compiler.Emit.ESModule as ESModule
import Ren.Compiler.Optimise.Declaration as Declaration
import Ren.Compiler.Optimise.Expression as Expression
import Ren.Compiler.Optimise.Module as Module
import Ren.Data.Declaration as Declaration
import Ren.Data.Expression as Expression
import Ren.Data.Module as Module



-- RUNNING THE COMPILER --------------------------------------------------------


{-| Run the entire compilation pipeline on a single input. This will parse the
input as a `Module`, perform a number of AST optimisations and transformations,
and then emit the module as an ES6 JavaScript module.

You can recreate this function yourself from a composition of the other functions
exposed in this module:

    import Ren.Compiler
        exposing
            ( Target(..)
            , emit
            , optimise
            , parse
            )

    compile =
        parse
            >> Result.map optimise
            >> Result.map (emit ESModule)

-}
compile : String -> Result (List Parser.DeadEnd) String
compile source =
    parse source
        |> Result.map optimise
        |> Result.map (emit ESModule)


{-| -}
compileTo : Target -> String -> Result (List Parser.DeadEnd) String
compileTo target source =
    parse source
        |> Result.map optimise
        |> Result.map (emit target)



-- TYPES -----------------------------------------------------------------------


{-| Modules are made up of two things. First, zero or more imports (separated by
newlines), and then zero or more declarations (also separated by new lines). A
basic module might look like:

    import 'Console' as Console

    pub fun main = _ => Console.log "Hello world!"

-}
type alias Module =
    Module.Module


{-| Declarations can either be functions (declared with the `fun` keyword), or
variables (declared with the `let` keyword). Additionally, declarations may be
marked as public exports with the `pub` keyword. Here are some declarations:

    fun foo = x y =>
        x + y * z
    where
        z = 0.99

    pub let bar = "Hello world!"

    let baz = fun a => a * 2

-}
type alias Declaration =
    Declaration.Declaration


{-| -}
type alias Expression =
    Expression.Expression



-- COMPILER STAGES: PARSING ----------------------------------------------------


{-| -}
parse : String -> Result (List Parser.DeadEnd) Module
parse =
    Module.fromSource


{-| -}
parseDeclaration : String -> Result (List Parser.DeadEnd) Declaration
parseDeclaration =
    Declaration.fromSource


{-| -}
parseExpression : String -> Result (List Parser.DeadEnd) Expression
parseExpression =
    Expression.fromSource


{-| -}
decode : Json.Decode.Value -> Result Json.Decode.Error Module
decode =
    Module.fromJSON


{-| -}
decodeDeclaration : Json.Decode.Value -> Result Json.Decode.Error Declaration
decodeDeclaration =
    Declaration.fromJSON


{-| -}
decodeExpression : Json.Decode.Value -> Result Json.Decode.Error Expression
decodeExpression =
    Expression.fromJSON



-- COMPILER STAGES: OPTIMISATION ------------------------------------------------


{-| -}
optimise : Module -> Module
optimise =
    Module.optimise


{-| -}
optimiseDeclaration : Declaration -> Declaration
optimiseDeclaration =
    Declaration.optimise


{-| -}
optimiseExpression : Expression -> Expression
optimiseExpression =
    Expression.optimise



-- COMPILER STAGES: EMITTING ---------------------------------------------------


{-| -}
type Target
    = ESModule
    | CommonJS


{-| -}
emit : Target -> Module -> String
emit target =
    case target of
        ESModule ->
            ESModule.fromModule

        CommonJS ->
            CommonJS.fromModule


{-| -}
emitDeclaration : Target -> Declaration -> String
emitDeclaration target =
    case target of
        ESModule ->
            ESModule.fromDeclaration

        CommonJS ->
            CommonJS.fromDeclaration


{-| -}
emitExpression : Target -> Expression -> String
emitExpression target =
    case target of
        ESModule ->
            ESModule.fromExpression

        CommonJS ->
            CommonJS.fromExpression
