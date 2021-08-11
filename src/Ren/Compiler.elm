module Ren.Compiler exposing
    ( compile, compileTo
    , Module, Declaration, Expression
    , parse, parseDeclaration, parseExpression
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

---


## Optimisation

@docs optimise, optimiseDeclaration, optimiseExpression

---


## Emitting Target Code

@docs Target, emit, emitDeclaration, emitExpression

-}

-- IMPORTS ---------------------------------------------------------------------

import Parser
import Ren.Compiler.Emit.ESModule as ESModule
import Ren.Compiler.Optimise
import Ren.Compiler.Parse as Parse
import Ren.Language



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
    Ren.Language.Module


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
    Ren.Language.Declaration


{-| -}
type alias Expression =
    Ren.Language.Expression



-- COMPILER STAGES: PARSING ----------------------------------------------------


{-| -}
parse : String -> Result (List Parser.DeadEnd) Module
parse =
    Parse.moduleFromSource


{-| -}
parseDeclaration : String -> Result (List Parser.DeadEnd) Declaration
parseDeclaration =
    Parse.declarationFromSource


{-| -}
parseExpression : String -> Result (List Parser.DeadEnd) Expression
parseExpression =
    Parse.expressionFromSource



-- COMPILER STAGES: OPTIMISATION ------------------------------------------------


{-| -}
optimise : Module -> Module
optimise { imports, declarations } =
    { imports = imports
    , declarations =
        List.map (Tuple.mapSecond optimiseDeclaration) declarations
    }


{-| -}
optimiseDeclaration : Declaration -> Declaration
optimiseDeclaration declaration =
    case declaration of
        Ren.Language.Function name args expr ->
            Ren.Language.Function name args (optimiseExpression expr)

        Ren.Language.Variable name expr ->
            Ren.Language.Variable name (optimiseExpression expr)

        Ren.Language.Enum name variants ->
            Ren.Language.Enum name variants


{-| -}
optimiseExpression : Expression -> Expression
optimiseExpression =
    Ren.Compiler.Optimise.optimiseExpression



-- COMPILER STAGES: EMITTING ---------------------------------------------------


{-| -}
type Target
    = ESModule


{-| -}
emit : Target -> Module -> String
emit target =
    case target of
        ESModule ->
            ESModule.emitModule


{-| -}
emitDeclaration : Target -> Declaration -> String
emitDeclaration target =
    case target of
        ESModule ->
            ESModule.emitDeclaration


{-| -}
emitExpression : Target -> Expression -> String
emitExpression target =
    case target of
        ESModule ->
            ESModule.emitExpression
