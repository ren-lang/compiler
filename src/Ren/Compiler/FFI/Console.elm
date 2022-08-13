module Ren.Compiler.FFI.Console exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Compiler.FFI exposing (Task)


log : List String -> Task ()
log msgs =
    Ren.Compiler.FFI.call "Console" "log" (List.map Json.Encode.string msgs) (Json.Decode.succeed ())


dir : List String -> Task ()
dir msgs =
    Ren.Compiler.FFI.call "Console" "dir" [ Json.Encode.list Json.Encode.string msgs ] (Json.Decode.succeed ())
