module Ren.Samples.BottlesOfBeer exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Parser
import Ren.Compiler as Ren



-- SOURCE ----------------------------------------------------------------------


source : String
source =
    """
import 'ren/array' as Array
import 'ren/debug' as Debug

// The `main` function is always the entry point to your program when being run
// from Node. It receives `process.argv` as its only argument. We don't need it
// for this program though, so we can safely ignore it!
pub fun main = _ =>  {
    // Declarations may be nested, but they *must all be declared at once*.
    // Ren has no statements, so a function like this is always a list of
    // declarations and then a single expression to be returned.
    fun makeVerses = n => if n >= 0 then verse n :: makeVerses (n - 1) else []
    let verses = makeVerses numberOfBottles

    ret verses |> Array.forEach Debug.log
}

// Order of declaration is not significant in Ren, so we can declare the value
// here but use it in `main` no problem.
let numberOfBottles = 99

// When a function has no local declarations, we can omit the curly braces and
// the `ret` keyword and just write the return expression instead.
fun verse = n => when n
    is 0 =>
        'No more bottles of beer on the wall, no more bottles of beer. ' +
        'Go to the store and buy some more, 99 bottles of beer on the wall.'

    is _ =>
        bottles n + ' of beer on the wall, ' + bottles n + ' of beer. ' +
        'Take one down and pass it around, ' + bottles (n - 1) + ' of beer on the wall.'


fun bottles = n => when n
    is 0 => 'no more bottles'
    is 1 => '1 bottle'
    else => n + ' bottles'
"""


ast : Result (List Parser.DeadEnd) Ren.Module
ast =
    Ren.parse source
