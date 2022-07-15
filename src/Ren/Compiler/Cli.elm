port module Ren.Compiler.Cli exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Node.Chalk exposing (Chalk)
import Node.Fs exposing (Fs)
import Node.Gitly exposing (Gitly)
import Node.Path exposing (Path)
import Node.Process exposing (Process)
import Process
import Ren.Ast.Core as Core
import Ren.Ast.Expr as Expr
import Ren.Data.Declaration as Declaration
import Ren.Data.Module as Module
import Ren.Stage.Emit exposing (emit)
import Ren.Stage.Lex exposing (lex)
import Ren.Stage.Parse exposing (parseExpr)
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

        "make" :: root :: _ ->
            exit 0

        "make" :: [] ->
            exit 0

        "run" :: args ->
            exit 0

        "add" :: pkg :: _ ->
            exit 0

        "repl" :: [] ->
            exit 0

        "eval" :: "--src" :: src :: _ ->
            path.resolve [ process.cwd (), src ]
                |> (\path_ -> fs.readFile path_ "utf-8")
                |> Result.fromMaybe ()
                |> Result.andThen lex
                |> Result.andThen parseExpr
                |> Result.map (Expr.desugar >> Expr.Lambda [ Core.PAny ])
                |> Result.map (\expr -> Module.addLocalDeclaration True "$eval" expr Module.empty)
                |> Result.map (emit 80 { name = "$eval", root = process.cwd (), includeFFI = False })
                |> Result.map eval
                |> Result.withDefault (exit 0)

        "eval" :: str :: _ ->
            lex str
                |> Result.andThen parseExpr
                |> Result.map (Expr.desugar >> Expr.Lambda [ Core.PAny ])
                |> Result.map (\expr -> Module.addLocalDeclaration True "$eval" expr Module.empty)
                |> Result.map (emit 80 { name = "$eval", root = process.cwd (), includeFFI = False })
                |> Result.map eval
                |> Result.withDefault (exit 0)

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
