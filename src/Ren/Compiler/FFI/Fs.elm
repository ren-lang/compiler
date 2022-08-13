module Ren.Compiler.FFI.Fs exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Compiler.FFI exposing (Task)



--


{-| Synchronously copies src to dest. By default, dest is overwritten if it already
exists. Node.js makes no guarantees about the atomicity of the copy operation.
If an error occurs after the destination file has been opened for writing,
Node.js will attempt to remove the destination.
-}
copyFile : String -> String -> Task ()
copyFile src dest =
    Ren.Compiler.FFI.call "Fs"
        "copyFile"
        (List.map Json.Encode.string [ src, dest ])
        (Json.Decode.succeed ())


exists : String -> Task Bool
exists path =
    Ren.Compiler.FFI.call "Fs"
        "exists"
        [ Json.Encode.string path ]
        Json.Decode.bool


isDir : String -> Task Bool
isDir path =
    Ren.Compiler.FFI.call "Fs"
        "isDir"
        [ Json.Encode.string path ]
        Json.Decode.bool


isFile : String -> Task Bool
isFile path =
    Ren.Compiler.FFI.call "Fs"
        "isFile"
        [ Json.Encode.string path ]
        Json.Decode.bool


makeDir : Bool -> String -> Task ()
makeDir recursive path =
    Ren.Compiler.FFI.call "Fs"
        "mkdir"
        [ Json.Encode.string path, Json.Encode.object [ ( "recursive", Json.Encode.bool recursive ) ] ]
        (Json.Decode.succeed ())


readDir : String -> Task (List String)
readDir path =
    Ren.Compiler.FFI.call "Fs"
        "readdir"
        [ Json.Encode.string path, Json.Encode.object [ ( "encoding", Json.Encode.string "utf8" ) ] ]
        (Json.Decode.list Json.Decode.string)


readFile : String -> String -> Task String
readFile path encoding =
    Ren.Compiler.FFI.call "Fs"
        "readFile"
        [ Json.Encode.string path, Json.Encode.object [ ( "encoding", Json.Encode.string encoding ) ] ]
        Json.Decode.string


writeFile : String -> String -> Task ()
writeFile path data =
    Ren.Compiler.FFI.call "Fs"
        "writeFile"
        [ Json.Encode.string path, Json.Encode.string data, Json.Encode.object [ ( "encoding", Json.Encode.string "utf8" ) ] ]
        (Json.Decode.succeed ())
