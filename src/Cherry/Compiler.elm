module Cherry.Compiler exposing 
    ( run
    , Module, Declaration, Expression
    , parse, parseDeclaration, parseExpression
    , decode, decodeDeclaration, decodeExpression
    , optimise, optimiseDeclaration, optimiseExpression
    , Target(..), emit, emitDeclaration, emitExpression
    )


{-| 

@docs run

---
## Types

This module aliases the main types that you might be interested in working with.
See the appropriate modules (e.g [`Cherry.Data.Expression`](./Cherry-Data-Expression))
for more documentation about that type and the things you might want to do with
it.

The alises are provided here as a convenience so you only need to import
`Cherry.Compiler` if you don't want to do anything fancy!

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


import Cherry.Compiler.Optimise.Declaration as Declaration
import Cherry.Compiler.Optimise.Expression as Expression
import Cherry.Compiler.Emit.ES6 as ES6
import Cherry.Data.Expression as Expression
import Cherry.Data.Declaration as Declaration
import Cherry.Data.Module as Module
import Json.Decode
import Parser


-- RUNNING THE COMPILER --------------------------------------------------------


{-| Run the entire compilation pipeline on a single input. This will parse the
input as a `Module`, perform a number of AST optimisations and transformations,
and then emit the module as an ES6 JavaScript module.

You can recreate this function yourself from a composition of the other functions
exposed in this module:

    import Cherry.Compiler exposing 
        ( parse
        , optimise
        , emit, Target(..)
        )

    run = parse 
        >> Result.map optimise 
        >> Result.map (emit ES6)
-}
run : String -> Result (List Parser.DeadEnd) String
run source =
    parse source
        |> Result.map optimise 
        |> Result.map (emit ES6)


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
optimise (Module.Module ({ declarations } as data)) =
    Module.Module { data | declarations = List.map Declaration.optimise declarations }


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
    = ES6

{-| -}
emit : Target -> Module -> String
emit target =
    case target of
        ES6 ->
            ES6.fromModule

{-| -}
emitDeclaration : Target -> Declaration -> String
emitDeclaration target =
    case target of
        ES6 ->
            ES6.fromDeclaration

{-| -}
emitExpression : Target -> Expression -> String
emitExpression target =
    case target of
        ES6 ->
            ES6.fromExpression
