port module Ren.Compiler.Cli exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Node.Chalk exposing (Chalk)
import Node.Fs exposing (Fs)
import Node.Gitly exposing (Gitly)
import Node.Path exposing (Path)
import Node.Process exposing (Process)
import Process
import Ren.Ast.Core as Core
import Ren.Ast.Expr as Expr
import Ren.Ast.Mod as Mod
import Ren.Data.Token as Token
import Ren.Stage.Emit as Emitter
import Ren.Stage.Lex as Lexer
import Ren.Stage.Parse as Parser
import Task



-- RUNNING THE CLI -------------------------------------------------------------


{-| -}
type alias FFI =
    { chalk : Chalk
    , fs : Fs
    , gitly : Gitly
    , path : Path
    , process : Process
    }


{-| -}
run : FFI -> Cmd Int
run ({ chalk, fs, path, process } as ffi) =
    -- The first two elements in `argv` are the path of the Node executable and
    -- the path of the JavaScript file being executed respectively. I think we can
    -- safely ignore those, so we'll just drop them instead of pattern matching
    -- them.
    case List.drop 2 process.argv of
        "new" :: name :: _ ->
            exit 0

        "make" :: root :: args ->
            make ffi args <| ffi.path.resolve [ ffi.process.cwd (), root ]

        "make" :: [] ->
            make ffi [] <| ffi.process.cwd ()

        "run" :: args ->
            exit 0

        "add" :: pkg :: _ ->
            exit 0

        "repl" :: [] ->
            exit 0

        "eval" :: str :: args ->
            let
                stream =
                    Lexer.lex str

                ast =
                    Result.andThen Parser.parseExpr stream

                javascript =
                    ast
                        |> Result.map (Expr.desugar >> Expr.Lambda [ Core.PAny ])
                        |> Result.map (\expr -> Mod.addLocalDec True "$eval" expr Mod.empty)
                        |> Result.map (Emitter.emit 80 { name = "$eval", root = process.cwd (), includeFFI = False })
            in
            if List.member "--dump-tokens" args then
                Cmd.batch
                    [ stream
                        |> Result.map (stdout << Token.debug)
                        |> Result.withDefault (stderr "lexer error")
                    , exit 0
                    ]

            else if List.member "--dump-ast" args then
                Cmd.batch
                    [ ast
                        |> Result.map (stdout << Json.Encode.encode 2 << Expr.encode)
                        |> Result.withDefault (stderr "parser error")
                    , exit 0
                    ]

            else
                Cmd.batch
                    [ javascript
                        |> Result.map eval
                        |> Result.withDefault Cmd.none
                    , exit 0
                    ]

        command :: _ ->
            exitWithError 1 <|
                String.join "\n"
                    [ chalk.red "[Unknown Command] I didn't recognise that command, did you mean one of these:"
                    , ""
                    , "  - ren " ++ chalk.green "new " ++ " <project_name>"
                    , "  - ren " ++ chalk.green "make"
                    , "  - ren " ++ chalk.green "run " ++ " <file_name>"
                    , "  - ren " ++ chalk.green "add " ++ " <package_name>"
                    , "  - ren " ++ chalk.green "eval" ++ " <expr>"
                    , ""
                    ]

        [] ->
            exitWithMessage 0 <|
                String.join "\n"
                    [ chalk.red "[Unknown Command] Here's everything I can do:"
                    , ""
                    , "  - ren " ++ chalk.green "new " ++ " <project_name>"
                    , "  - ren " ++ chalk.green "make"
                    , "  - ren " ++ chalk.green "run " ++ " <file_name>"
                    , "  - ren " ++ chalk.green "add " ++ " <package_name>"
                    , "  - ren " ++ chalk.green "eval" ++ " <expr>"
                    , ""
                    ]



-- COMMANDS --------------------------------------------------------------------


make : FFI -> List String -> String -> Cmd Int
make ffi args root =
    if ffi.fs.isFile root then
        makeFile ffi args root

    else
        ffi.fs.readDir root
            |> List.map
                (\dirent ->
                    let
                        path =
                            ffi.path.join [ root, dirent ]
                    in
                    if ffi.fs.isFile path then
                        makeFile ffi args path

                    else
                        make ffi args path
                )
            |> Cmd.batch


makeFile : FFI -> List String -> String -> Cmd Int
makeFile ffi args path =
    let
        out =
            path ++ ".js"

        meta =
            { name = ffi.path.basename path <| Just ".ren"
            , root = ""
            , includeFFI = True
            }
    in
    if String.endsWith ".ren" path then
        if List.member "--dump-tokens" args then
            ffi.fs.readFile path "utf-8"
                |> Result.fromMaybe ()
                |> Result.andThen Lexer.lex
                |> Result.map Token.debug
                |> Result.map (ffi.fs.writeFile <| path ++ ".json")
                |> Result.map (\_ -> Cmd.none)
                |> Result.withDefault (stderr <| "error compiling `" ++ path ++ "`")

        else if List.member "--dump-ast" args then
            ffi.fs.readFile path "utf-8"
                |> Result.fromMaybe ()
                |> Result.andThen Lexer.lex
                |> Result.andThen Parser.parse
                |> Result.map (Mod.encode >> Json.Encode.encode 2)
                |> Result.map (ffi.fs.writeFile <| path ++ ".json")
                |> Result.map (\_ -> Cmd.none)
                |> Result.withDefault (stderr <| "error compiling `" ++ path ++ "`")

        else
            ffi.fs.readFile path "utf-8"
                |> Result.fromMaybe ()
                |> Result.andThen Lexer.lex
                |> Result.andThen Parser.parse
                |> Result.map (Emitter.emit 80 meta)
                |> Result.map (ffi.fs.writeFile out)
                |> Result.map (\_ -> Cmd.none)
                |> Result.withDefault (stderr <| "error compiling `" ++ path ++ "`")

    else
        Cmd.none



-- EXITS AND ERROR HANDLING ----------------------------------------------------


{-| -}
exit : Int -> Cmd Int
exit code =
    -- This sleep is necessary to stop Elm from synchronously calling `update`
    -- and exiting immediately.
    Task.perform (\_ -> code) (Process.sleep 0)


{-| -}
exitWithMessage : Int -> String -> Cmd Int
exitWithMessage code message =
    Cmd.batch [ exit code, stdout message ]


{-| -}
exitWithError : Int -> String -> Cmd Int
exitWithError code message =
    Cmd.batch [ exit code, stderr message ]



-- MAIN ------------------------------------------------------------------------


main : Program Json.Decode.Value (Maybe FFI) Int
main =
    Platform.worker
        { init =
            \flags ->
                let
                    ffiDecoder =
                        Json.Decode.map5 FFI
                            (Json.Decode.field "chalk" Node.Chalk.decoder)
                            (Json.Decode.field "fs" Node.Fs.decoder)
                            (Json.Decode.field "gitly" Node.Gitly.decoder)
                            (Json.Decode.field "path" Node.Path.decoder)
                            (Json.Decode.field "process" Node.Process.decoder)
                in
                case Json.Decode.decodeValue ffiDecoder flags of
                    Ok ffi ->
                        ( Just ffi, run ffi )

                    Err _ ->
                        ( Nothing
                        , stderr <|
                            "Uh oh, it looks like there was an internal error while trying to "
                                ++ "initialise some FFI code. Please open an issue at "
                                ++ "https://github.com/ren-lang/compiler."
                        )
        , update =
            \code ffi ->
                case ffi of
                    Just { process } ->
                        ( Basics.always ffi <| process.exit code, Cmd.none )

                    Nothing ->
                        ( ffi, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }



-- PORTS -----------------------------------------------------------------------


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


port exec : ( String, List String ) -> Cmd msg


port eval : String -> Cmd msg
