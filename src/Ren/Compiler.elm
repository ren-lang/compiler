module Ren.Compiler exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Data.List
import Data.Tuple2
import Parser.Advanced as Parser
import Ren.AST.Module as Module exposing (Module)
import Ren.Compiler.Check as Check
import Ren.Compiler.Desugar as Desugar
import Ren.Compiler.Emit.ESModule as ESModule
import Ren.Compiler.Emit.Ren as Ren
import Ren.Compiler.Optimise as Optimise
import Ren.Compiler.Parse as Parse
import Ren.Data.Polyenv as Polyenv
import Ren.Data.Span exposing (Span)
import Ren.Data.Type as Type
import Ren.Data.Typing as Typing



-- TYPES -----------------------------------------------------------------------


type alias Compiler error =
    String -> Result error String


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


{-| -}
type Target
    = ESModule
    | Ren
    | Types



-- TOOLCHAINS ------------------------------------------------------------------


{-| -}
untyped : Toolchain Error Span
untyped =
    custom False Desugar.defaults [ Optimise.operators ] ESModule


{-| -}
typed : Toolchain Error Span
typed =
    custom True Desugar.defaults [ Optimise.operators ] ESModule


{-| This toolchain is essentially the identity function on well-formatted Ren code.
It simply parses the input and then re-emits Ren code ran through the pretty
printer.

Importantly, it doesn't run the desugar or optimise phases that would otherwise
transform the code in potentially surprising ways.

-}
format : Toolchain Error Span
format =
    custom False [] [] Ren
        |> (\toolchain ->
                { toolchain
                  -- We want to run the type checker (to auto-insert type annotations)
                  -- but we also don't want formatting to fail just because type
                  -- checking did, so we'll run the type checker but fall back
                  -- to the original declaration (and thus the default `*` type)
                  -- if it fails.
                    | check =
                        \({ declarations } as m) ->
                            let
                                polyenv =
                                    List.foldl
                                        (\{ name, type_ } env ->
                                            Polyenv.insert name (Typing.poly type_) env
                                        )
                                        Polyenv.empty
                                        declarations
                            in
                            List.map (Check.declaration polyenv) declarations
                                |> Data.List.zip declarations
                                |> List.map (Data.Tuple2.apply Result.withDefault)
                                |> (\ds -> { m | declarations = ds })
                                |> Ok
                }
           )


{-| This toolchain doesn't emit code at the end; it type checks a module and then
emits a list of declarations and their type. Of course, declarations must be
type annotated in order to be type checked at all so running this toolchain
doesn't provide you with any information you didn't already know, but it is handy
to have around to test the type checker is working without flooding the console
with emitted pretty-printed code.
-}
typecheck : Toolchain Error Span
typecheck =
    custom True Desugar.defaults [] Types


{-| -}
custom : Bool -> List (Desugar.Transformation Span) -> List (Optimise.Optimisation Span) -> Target -> Toolchain Error Span
custom shouldTypecheck transformations optimisations target =
    { parse = Parse.run >> Result.mapError ParseError
    , desugar = Module.map (Desugar.run transformations)
    , validate = Ok
    , check =
        if shouldTypecheck then
            \({ declarations } as m) ->
                let
                    polyenv =
                        List.foldl (\{ name, type_ } env -> Polyenv.insert name (Typing.poly type_) env) Polyenv.empty declarations
                in
                List.foldr (\d ds -> Result.map2 (::) (Check.declaration polyenv d) ds) (Ok []) declarations
                    |> Result.mapError TypeError
                    |> Result.map (\ds -> { m | declarations = ds })

        else
            Ok
    , optimise = Module.map (Optimise.run optimisations)
    , emit =
        case target of
            ESModule ->
                ESModule.run

            Ren ->
                Ren.run

            -- This one is a bit ad-hoc. We don't have a proper Emit.* module for
            -- this target beacuse it's really just for debugging.
            Types ->
                let
                    showDeclaration { name, type_ } =
                        name ++ " : " ++ (Type.toString <| Type.reduce type_)
                in
                .declarations >> List.map showDeclaration >> String.join "\n\n"
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



--


testInput : String
testInput =
    """
import "ren/array" as Array
import "ren/console" as Console exposing { log }
import "ren/file" as File
import "ren/string" as String
import "ren/result" as Result

pub let main = [ year, day, part, test ] => {
    let path = if test then "test.txt" else "input.txt"

    ret File.open path { sync: true } 
        |> Result.map (String.split " " >> Array.filterMap String.toNumber)
        |> Result.andThen (solve part)
}

let solve = part input => where part
        is "01" => #ok (solvePartOne input)
        is "02" => #ok (solvePartTwo input)
        is _    => #err `Unknown part: "${part}".`


let solvePartOne = numbers =>
    Array.map2 (x y => if x < y then 1 else 0) numbers (Array.drop 1 numbers)
        |> Array.sum

let solvePartTwo = numbers =>
    Array.map3 (x y z => x + y + z) numbers (Array.drop 1 numbers) (Array.drop 2 numbers)
        |> solvePartOne
"""
