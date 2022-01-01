module Ren.Compiler exposing (..)

-- IMPORTS ---------------------------------------------------------------------

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



-- TOOLCHAINS ------------------------------------------------------------------


{-| -}
untyped : Target -> Toolchain Error Span
untyped target =
    { parse = Parse.run >> Result.mapError ParseError
    , desugar = Module.map (Desugar.run [ Desugar.placeholders ])
    , validate = Ok
    , check = Ok
    , optimise = Module.map (Optimise.run [ Optimise.operators ])
    , emit =
        case target of
            ESModule ->
                ESModule.run

            Ren ->
                Ren.run
    }


{-| -}
typed : Target -> Toolchain Error Span
typed target =
    { parse = Parse.run >> Result.mapError ParseError
    , desugar = Module.map (Desugar.run [ Desugar.placeholders ])
    , validate = Ok
    , check =
        \({ declarations } as m) ->
            let
                polyenv =
                    List.foldl (\{ name, type_ } env -> Polyenv.insert name (Typing.poly type_) env) Polyenv.empty declarations
            in
            List.foldr (\d ds -> Result.map2 (::) (Check.declaration polyenv d) ds) (Ok []) declarations
                |> Result.mapError TypeError
                |> Result.map (\ds -> { m | declarations = ds })
    , optimise = Module.map (Optimise.run [ Optimise.operators ])
    , emit =
        case target of
            ESModule ->
                ESModule.run

            Ren ->
                Ren.run
    }


{-| -}
format : Toolchain Error Span
format =
    { parse = Parse.run >> Result.mapError ParseError
    , desugar = Basics.identity
    , validate = Ok
    , check = Ok
    , optimise = Basics.identity
    , emit = Ren.run
    }



--


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
