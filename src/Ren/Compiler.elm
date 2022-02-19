module Ren.Compiler exposing
    ( Compiler, Toolchain, Error(..)
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
import Ren.AST.Module as Module exposing (Module)
import Ren.Compiler.Check as Check
import Ren.Compiler.Desugar as Desugar
import Ren.Compiler.Emit as Emit
import Ren.Compiler.Optimise as Optimise
import Ren.Compiler.Parse as Parse
import Ren.Data.Span exposing (Span)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Compiler error =
    String -> Result error String


{-| -}
type alias Toolchain error meta =
    { parse : String -> Result error (Module meta)
    , desugar : Module meta -> Module meta
    , validate : Module meta -> Result error (Module meta)
    , check : Module meta -> Result error (Module meta)
    , optimise : Module meta -> Module meta
    , emit : Module meta -> String
    }


{-| -}
type Error
    = ParseError (List (Parser.DeadEnd Parse.Context Parse.Error))
    | ValidationError
    | TypeError Check.Error



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
    { parse = Parse.run >> Result.mapError ParseError
    , desugar = Module.map (Desugar.run transformations)
    , validate = Ok
    , check =
        if shouldTypecheck then
            Check.run >> Result.mapError TypeError

        else
            Ok
    , optimise = Module.map (Optimise.run optimisations)
    , emit = Emit.run target
    }



--


{-| Chains together the various steps of a given toolchain to be run against
some Ren code input.
-}
run : Toolchain error meta -> Compiler error
run { parse, desugar, validate, check, optimise, emit } =
    parse
        >> Result.map desugar
        >> Result.andThen validate
        >> Result.andThen check
        >> Result.map optimise
        >> Result.map emit
