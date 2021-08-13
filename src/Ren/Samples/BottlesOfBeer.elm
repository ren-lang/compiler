module Ren.Samples.BottlesOfBeer exposing
    ( source
    , ast, output
    )

{-|

@docs source
@docs ast, output

-}

-- IMPORTS ---------------------------------------------------------------------

import Parser
import Ren.Compiler
import Ren.Language



-- SOURCE ----------------------------------------------------------------------


source : String
source =
    """
import 'ren/array' as Array exposing { forEach }
import 'ren/debug' as Debug
// The `main` function is always the entry point to your program when being run
// from Node. It receives `process.argv` as its only argument. We don't need it
// for this program though, so we can safely ignore it!
pub fun main = _ => {
    // Declarations may be nested, but they *must all be declared at once*.
    // Ren has no statements, so a function like this is always a list of
    // declarations and then a single expression to be returned.
    fun makeVerses = n => if n >= 0 then verse n :: makeVerses (n - 1) else []
    let verses = makeVerses numberOfBottles
    ret verses |> forEach Debug.log
}

// When a function has no local declarations, we can omit the curly braces and
// the `ret` keyword and just write the return expression instead. In fact, the
// curly braces are a block *expression* and have nothing to do with declarations
// at all!
fun verse = n => when n
    is 0 =>
        'No more bottles of beer on the wall, no more bottles of beer. ' +
        'Go to the store and buy some more, 99 bottles of beer on the wall.'

    is _ if n > 0 =>
        bottles n + ' of beer on the wall, ' + bottles n + ' of beer. ' +
        'Take one down and pass it around, ' + bottles (n - 1) + ' of beer on the wall.'


fun bottles = n => when n
    is 0 => 'no more bottles'
    is 1 => '1 bottle'
    else => n + ' bottles'
"""



-- OUTPUT ----------------------------------------------------------------------


ast : Result (List Parser.DeadEnd) Ren.Language.Module
ast =
    Ren.Compiler.parse source


output : String
output =
    Ren.Compiler.compile source
        |> Result.withDefault ""
