module Ren.Compiler exposing
    ( Toolchain, Error
    , run
    , untyped, typed, typecheck, custom
    , parse, desugar, check, optimise, emit
    )

{-|

@docs Compiler, Toolchain, Error
@docs run
@docs untyped, typed, typecheck, custom
@docs parse, desugar, check, optimise, emit

-}

-- IMPORTS ---------------------------------------------------------------------

import Ren.AST.Module as Module exposing (Import, Module)
import Ren.Compiler.Check as Check
import Ren.Compiler.Desugar as Desugar
import Ren.Compiler.Emit as Emit
import Ren.Compiler.Error as Error
import Ren.Compiler.Optimise as Optimise
import Ren.Compiler.Parse as Parse
import Ren.Data.Span exposing (Span)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Toolchain meta output =
    { parse : String -> Result Error (Module meta)
    , desugar : Module meta -> Module meta
    , validate : Module meta -> Result Error (Module meta)
    , check : Module meta -> Result Error (Module meta)
    , optimise : Module meta -> Module meta
    , emit : Module meta -> output
    }


{-| -}
type alias Error =
    Error.Error



-- TOOLCHAINS ------------------------------------------------------------------


{-| Chains together the various steps of a given toolchain to be run against
some Ren code input.
-}
run : Toolchain meta output -> String -> Result Error output
run toolchain input =
    toolchain.parse input
        |> Result.map toolchain.desugar
        |> Result.andThen toolchain.validate
        |> Result.andThen toolchain.check
        |> Result.map toolchain.optimise
        |> Result.map toolchain.emit


{-| -}
untyped : String -> Toolchain Span String
untyped moduleName =
    custom False Desugar.defaults [ Optimise.operators ] moduleName Emit.ESModule


{-| -}
typed : String -> Toolchain Span String
typed moduleName =
    custom True Desugar.defaults [ Optimise.operators ] moduleName Emit.ESModule


{-| This toolchain doesn't emit code at the end; it type checks a module and then
emits a list of declarations and their type. Of course, declarations must be
type annotated in order to be type checked at all so running this toolchain
doesn't provide you with any information you didn't already know, but it is handy
to have around to test the type checker is working without flooding the console
with emitted pretty-printed code.
-}
typecheck : Toolchain Span String
typecheck =
    custom True Desugar.defaults [] "" Emit.DEBUG_Types


{-| -}
custom : Bool -> List (Desugar.Transformation Span) -> List (Optimise.Optimisation Span) -> String -> Emit.Target -> Toolchain Span String
custom shouldTypecheck transformations optimisations name target =
    { parse = Parse.run name
    , desugar =
        \m ->
            if List.isEmpty <| Module.externs m then
                Module.map (Desugar.run transformations) m

            else
                { m | imports = Import Module.FfiImport [ "$FFI" ] [] :: m.imports }
                    |> Module.map (Desugar.run transformations)
    , validate = Ok
    , check =
        if shouldTypecheck then
            Check.run

        else
            Ok
    , optimise = Module.map (Optimise.run optimisations)
    , emit = Emit.run target
    }



-- INDIVIDUAL STEPS ------------------------------------------------------------


{-| -}
parse : String -> String -> Result Error (Module Span)
parse =
    Parse.run


{-| -}
desugar : List (Desugar.Transformation Span) -> Module Span -> Module Span
desugar transformations m =
    if List.isEmpty <| Module.externs m then
        Module.map (Desugar.run transformations) m

    else
        { m | imports = Import Module.FfiImport [ "$FFI" ] [] :: m.imports }
            |> Module.map (Desugar.run transformations)


{-| -}
check : Module Span -> Result Error (Module Span)
check =
    Check.run


{-| -}
optimise : List (Optimise.Optimisation Span) -> Module Span -> Module Span
optimise optimisations m =
    Module.map (Optimise.run optimisations) m


{-| -}
emit : Emit.Target -> Module Span -> String
emit =
    Emit.run
