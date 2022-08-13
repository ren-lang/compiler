module Ren.Compiler.FFI.Process exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Ren.Compiler.FFI exposing (Task)



-- TYPES -----------------------------------------------------------------------


type Architecture
    = ARM
    | ARM64
    | IA32
    | MIPS
    | MIPSEL
    | PPC
    | PPC64
    | S390
    | S390X
    | X64
      --
    | UnknownArchitecture


type Platform
    = AIX
    | Darwin
    | FreeBSD
    | Linux
    | OpenBSD
    | SunOS
    | Win32
      --
    | UnknownPlatform



--


abort : Task ()
abort =
    Ren.Compiler.FFI.call "Process" "abort" [] (Json.Decode.succeed ())


arch : Task Architecture
arch =
    Ren.Compiler.FFI.get "Process" "arch" <|
        (Json.Decode.string
            |> Json.Decode.map
                (\s ->
                    case s of
                        "arm" ->
                            ARM

                        "arm64" ->
                            ARM64

                        "ia32" ->
                            IA32

                        "mips" ->
                            MIPS

                        "mipsel" ->
                            MIPSEL

                        "ppc" ->
                            PPC

                        "ppc64" ->
                            PPC64

                        "s390" ->
                            S390

                        "s390x" ->
                            S390X

                        "x64" ->
                            X64

                        _ ->
                            UnknownArchitecture
                )
        )


argv : Task (List String)
argv =
    Ren.Compiler.FFI.get "Process" "argv" (Json.Decode.list Json.Decode.string)


chdir : String -> Task ()
chdir dir =
    Ren.Compiler.FFI.call "Process"
        "chdir"
        [ Json.Encode.string dir ]
        (Json.Decode.succeed ())


cwd : Task String
cwd =
    Ren.Compiler.FFI.call "Process" "cwd" [] Json.Decode.string


env : Task (Dict String String)
env =
    Ren.Compiler.FFI.get "Process" "env" (Json.Decode.dict Json.Decode.string)


exit : Int -> Task ()
exit code =
    Ren.Compiler.FFI.call "Process" "exit" [ Json.Encode.int code ] (Json.Decode.succeed ())


platform : Task Platform
platform =
    Ren.Compiler.FFI.get "Process" "platform" <|
        (Json.Decode.string
            |> Json.Decode.map
                (\s ->
                    case s of
                        "aix" ->
                            AIX

                        "darwin" ->
                            Darwin

                        "freebsd" ->
                            FreeBSD

                        "linux" ->
                            Linux

                        "openbsd" ->
                            OpenBSD

                        "sunos" ->
                            SunOS

                        "win32" ->
                            Win32

                        _ ->
                            UnknownPlatform
                )
        )
