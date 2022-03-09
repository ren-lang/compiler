module Ren.Compiler exposing
    ( Compiler, Toolchain, Error
    , run
    , untyped, typed, typecheck, custom
    )

{-|

@docs Compiler, Toolchain, Error
@docs run
@docs untyped, typed, typecheck, custom

-}

-- IMPORTS ---------------------------------------------------------------------

import Parser.Advanced as Parser
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
type alias Compiler error =
    String -> Result error String


{-| -}
type alias Toolchain error meta =
    { parse : String -> String -> Result error (Module meta)
    , desugar : Module meta -> Module meta
    , validate : Module meta -> Result error (Module meta)
    , check : Module meta -> Result error (Module meta)
    , optimise : Module meta -> Module meta
    , emit : Module meta -> String
    }


{-| -}
type alias Error =
    Error.Error



-- TOOLCHAINS ------------------------------------------------------------------


{-| -}
untyped : Toolchain Error Span
untyped =
    custom False Desugar.defaults [ Optimise.operators ] Emit.ESModule


{-| -}
typed : Toolchain Error Span
typed =
    custom True Desugar.defaults [ Optimise.operators ] Emit.ESModule


{-| This toolchain doesn't emit code at the end; it type checks a module and then
emits a list of declarations and their type. Of course, declarations must be
type annotated in order to be type checked at all so running this toolchain
doesn't provide you with any information you didn't already know, but it is handy
to have around to test the type checker is working without flooding the console
with emitted pretty-printed code.
-}
typecheck : Toolchain Error Span
typecheck =
    custom True Desugar.defaults [] Emit.DEBUG_Types


{-| -}
custom : Bool -> List (Desugar.Transformation Span) -> List (Optimise.Optimisation Span) -> Emit.Target -> Toolchain Error Span
custom shouldTypecheck transformations optimisations target =
    { parse = \name input -> Parse.run name input
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



--


{-| Chains together the various steps of a given toolchain to be run against
some Ren code input.
-}
run : String -> Toolchain error meta -> Compiler error
run name { parse, desugar, validate, check, optimise, emit } =
    parse name
        >> Result.map desugar
        >> Result.andThen validate
        >> Result.andThen check
        >> Result.map optimise
        >> Result.map emit
