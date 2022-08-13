module Ren.Compiler.FFI.Chalk exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Compiler.FFI exposing (Task)



--


black : String -> Task String
black input =
    Ren.Compiler.FFI.call "chalk"
        "black"
        [ Json.Encode.string input ]
        Json.Decode.string


red : String -> Task String
red input =
    Ren.Compiler.FFI.call "chalk"
        "red"
        [ Json.Encode.string input ]
        Json.Decode.string


green : String -> Task String
green input =
    Ren.Compiler.FFI.call "chalk"
        "green"
        [ Json.Encode.string input ]
        Json.Decode.string


yellow : String -> Task String
yellow input =
    Ren.Compiler.FFI.call "chalk"
        "yellow"
        [ Json.Encode.string input ]
        Json.Decode.string


blue : String -> Task String
blue input =
    Ren.Compiler.FFI.call "chalk"
        "blue"
        [ Json.Encode.string input ]
        Json.Decode.string


magenta : String -> Task String
magenta input =
    Ren.Compiler.FFI.call "chalk"
        "magenta"
        [ Json.Encode.string input ]
        Json.Decode.string


cyan : String -> Task String
cyan input =
    Ren.Compiler.FFI.call "chalk"
        "cyan"
        [ Json.Encode.string input ]
        Json.Decode.string


white : String -> Task String
white input =
    Ren.Compiler.FFI.call "chalk"
        "white"
        [ Json.Encode.string input ]
        Json.Decode.string


grey : String -> Task String
grey =
    gray


gray : String -> Task String
gray input =
    Ren.Compiler.FFI.call "chalk"
        "gray"
        [ Json.Encode.string input ]
        Json.Decode.string
