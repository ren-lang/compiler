module Node.Process exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Util.FFI



-- TYPES -----------------------------------------------------------------------


{-| -}
type Proxy
    = Proxy Json.Decode.Value


{-| -}
type alias Process =
    { abort : () -> ()
    , arch : Architecture
    , argv : List String
    , chdir : String -> ()
    , cwd : () -> String
    , env : Dict String String
    , exit : Int -> ()
    , platform : Platform

    --
    , this : Proxy
    }


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



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
decoder : Json.Decode.Decoder Process
decoder =
    Json.Decode.map2 Tuple.pair (Json.Decode.map Proxy Json.Decode.value) (Json.Decode.field "FFI.Process" Json.Decode.bool)
        |> Json.Decode.andThen
            (\( proxy, isProcessProxy ) ->
                if isProcessProxy then
                    Json.Decode.succeed
                        { abort = proxy |> abort
                        , arch = proxy |> arch
                        , argv = proxy |> argv
                        , chdir = proxy |> chdir
                        , cwd = proxy |> cwd
                        , env = proxy |> env
                        , exit = proxy |> exit
                        , platform = proxy |> platform

                        --
                        , this = proxy
                        }

                else
                    Json.Decode.fail <|
                        "Uh oh, it looks like there was an internal error in "
                            ++ "the `FFI.Process.elm` module. Please open an issue at "
                            ++ "https://github.com/ren-lang/compiler."
            )



--


abort : Proxy -> () -> ()
abort (Proxy proxy) _ =
    Util.FFI.call proxy "abort" [] (Json.Decode.succeed ())
        |> Maybe.withDefault ()


arch : Proxy -> Architecture
arch (Proxy proxy) =
    case Util.FFI.get proxy "arch" Json.Decode.string of
        Just "arm" ->
            ARM

        Just "arm64" ->
            ARM64

        Just "ia32" ->
            IA32

        Just "mips" ->
            MIPS

        Just "mipsel" ->
            MIPSEL

        Just "ppc" ->
            PPC

        Just "ppc64" ->
            PPC64

        Just "s390" ->
            S390

        Just "s390x" ->
            S390X

        Just "x64" ->
            X64

        _ ->
            UnknownArchitecture


argv : Proxy -> List String
argv (Proxy proxy) =
    Util.FFI.get proxy "argv" (Json.Decode.list Json.Decode.string)
        |> Maybe.withDefault []


chdir : Proxy -> String -> ()
chdir (Proxy proxy) dir =
    Maybe.withDefault () <|
        Util.FFI.call proxy
            "chdir"
            [ Json.Encode.string dir ]
            (Json.Decode.succeed ())


cwd : Proxy -> () -> String
cwd (Proxy proxy) _ =
    Util.FFI.call proxy "cwd" [] Json.Decode.string
        |> Maybe.withDefault ""


env : Proxy -> Dict String String
env (Proxy proxy) =
    Util.FFI.get proxy "env" (Json.Decode.dict Json.Decode.string)
        |> Maybe.withDefault Dict.empty


exit : Proxy -> Int -> ()
exit (Proxy proxy) code =
    Util.FFI.call proxy "exit" [ Json.Encode.int code ] (Json.Decode.succeed ())
        |> Maybe.withDefault ()


platform : Proxy -> Platform
platform (Proxy proxy) =
    case Util.FFI.get proxy "platform" Json.Decode.string of
        Just "aix" ->
            AIX

        Just "darwin" ->
            Darwin

        Just "freebsd" ->
            FreeBSD

        Just "linux" ->
            Linux

        Just "openbsd" ->
            OpenBSD

        Just "sunos" ->
            SunOS

        Just "win32" ->
            Win32

        _ ->
            UnknownPlatform
