port module Ren.Compiler.CLI exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Ast.Mod as Mod
import Ren.Ast.Mod.Meta as Meta
import Ren.Compiler.FFI as FFI
import Ren.Compiler.FFI.Console as Console
import Ren.Compiler.FFI.Fs as Fs
import Ren.Compiler.FFI.Path as Path
import Ren.Compiler.FFI.Process as Process
import Ren.Compiler.Project as Project
import Ren.Stage.Emit.JavaScript as Emit
import Ren.Stage.Parse as Parse
import Task
import Util.Result as Result
import Util.Task as Task


type alias Argv =
    List String


init : Argv -> FFI.Task ()
init argv =
    case argv of
        [ "init" ] ->
            FFI.do Process.cwd <| \cwd ->
            FFI.do (Path.join [ cwd, "ren.json" ]) <| \configPath ->
            FFI.do (Fs.exists configPath) <| \exists ->
            if exists then
                Task.fail "ren.json already exists"

            else
                -- Create ren.json
                FFI.do (Fs.writeFile configPath <| Project.toJson Project.init) <| \_ ->
                -- Create .ren/ dir
                FFI.do (Path.join [ cwd, ".ren" ]) <| \renPath ->
                FFI.do (Fs.makeDir True renPath) <| \_ ->
                FFI.do (Fs.makeDir True <| renPath ++ "/pkg") <| \_ ->
                -- Create src/ dir
                FFI.do (Path.join [ cwd, "src" ]) <| \srcPath ->
                FFI.do (Fs.makeDir True srcPath) <| \_ ->
                Task.succeed ()

        [ "init", "." ] ->
            FFI.do Process.cwd <| \cwd ->
            FFI.do (Path.join [ cwd, "ren.json" ]) <| \configPath ->
            FFI.do (Fs.exists configPath) <| \exists ->
            if exists then
                Task.fail "ren.json already exists"

            else
                -- Create ren.json
                FFI.do (Fs.writeFile configPath <| Project.toJson Project.init) <| \_ ->
                -- Create .ren/ dir
                FFI.do (Path.join [ cwd, ".ren" ]) <| \renPath ->
                FFI.do (Fs.makeDir True renPath) <| \_ ->
                FFI.do (Fs.makeDir True <| renPath ++ "/pkg") <| \_ ->
                -- Create src/ dir
                FFI.do (Path.join [ cwd, "src" ]) <| \srcPath ->
                FFI.do (Fs.makeDir True srcPath) <| \_ ->
                Task.succeed ()

        [ "init", name ] ->
            FFI.do Process.cwd <| \cwd ->
            FFI.do (Path.join [ cwd, name ]) <| \projectPath ->
            FFI.do (Fs.exists projectPath) <| \exists ->
            if exists then
                Task.fail <| "A directory named " ++ name ++ " already exists."

            else
                FFI.do (Fs.makeDir True projectPath) <| \_ ->
                FFI.do (Path.join [ projectPath, "ren.json" ]) <| \configPath ->
                FFI.do (Fs.writeFile configPath <| Project.toJson Project.init) <| \_ ->
                FFI.do (Path.join [ projectPath, ".ren" ]) <| \renPath ->
                FFI.do (Fs.makeDir True renPath) <| \_ ->
                FFI.do (Fs.makeDir True <| renPath ++ "/pkg") <| \_ ->
                FFI.do (Path.join [ projectPath, "src" ]) <| \srcPath ->
                FFI.do (Fs.makeDir True srcPath) <| \_ ->
                Task.succeed ()

        [ "make" ] ->
            FFI.do Process.cwd <| \cwd ->
            FFI.do (Path.join [ cwd, "ren.json" ]) <| \configPath ->
            FFI.do (Fs.exists configPath) <| \exists ->
            if exists then
                FFI.do (Path.join [ cwd, ".ren", "pkg" ]) <| \pkgPath ->
                FFI.do (Path.join [ cwd, "src" ]) <| \srcPath ->
                -- Write the prelude module.
                FFI.do (Fs.writeFile (pkgPath ++ "/prelude.js") Emit.prelude) <| \_ ->
                -- Recursively read every `*.ren` file in the project.
                FFI.do (readFiles srcPath) <| \files ->
                Dict.toList files
                    |> Task.traverse
                        (\( filePath, src ) ->
                            -- Get the file name without the extension.
                            FFI.do (Path.basename filePath <| Just ".ren") <| \filename ->
                            -- Get the file path without the file name.
                            FFI.do (Path.dirname filePath) <| \filedir ->
                            -- Work out relative path from this module to the
                            -- root directory for package imports.
                            FFI.do (Path.relative filedir pkgPath) <| \absPkgPath ->
                            -- Work out the relative path of this module from the
                            -- current work directory
                            FFI.do (Path.relative cwd filePath) <| \relativeFilepath ->
                            Parse.mod src
                                |> Result.map (Mod.transformMeta (Meta.setName filename))
                                |> Result.map (Mod.transformMeta (Meta.setPkgPath absPkgPath))
                                |> Result.map Emit.mod
                                |> Result.map
                                    (\mod ->
                                        FFI.do (Console.log [ "✅ Sucessfully compiled " ++ relativeFilepath ]) <| \_ ->
                                        Fs.writeFile (filePath ++ ".js") mod
                                    )
                                |> Result.mapError
                                    (\err ->
                                        FFI.do (Console.log [ "❎ Error compiling " ++ relativeFilepath ]) <| \_ ->
                                        Task.fail err
                                    )
                                |> Result.unwrap
                        )
                    |> Task.replace ()

            else
                Task.fail "ren.json not found, try `ren init` first"

        _ ->
            Task.succeed ()


readFiles : String -> FFI.Task (Dict String String)
readFiles path =
    FFI.do (Fs.isDir path) <| \isDir ->
    if isDir then
        FFI.do (Fs.readDir path) <| \entries ->
        FFI.do (Task.traverse (\entry -> Path.join [ path, entry ]) entries) <| \paths ->
        FFI.do (Task.traverse readFiles paths) <| \files ->
        Task.succeed <| List.foldl Dict.union Dict.empty files

    else if String.endsWith ".ren" path then
        FFI.do (Fs.readFile path "utf-8") <| \src ->
        Task.succeed <| Dict.singleton path src

    else
        Task.succeed Dict.empty



-- MAIN ------------------------------------------------------------------------


main : Program Argv () (Result String ())
main =
    Platform.worker
        { init =
            \argv ->
                ( (), Task.attempt Basics.identity (init argv) )
        , update =
            \result _ ->
                case result of
                    Ok _ ->
                        ( (), Cmd.none )

                    Err message ->
                        ( (), stderr message )
        , subscriptions = Basics.always Sub.none
        }


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg
