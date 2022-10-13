port module Ren.Compiler.CLI exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Ast.Mod exposing (Mod)
import Ren.Compiler.FFI.Console as Console
import Ren.Compiler.FFI.Fs as Fs
import Ren.Compiler.FFI.Path as Path
import Ren.Compiler.FFI.Process as Process
import Ren.Control.Query as Query exposing (Query)
import Ren.Control.Query.Env as Env
import Ren.Data.Error exposing (Error)
import Ren.Queries.Emit as Emit
import Ren.Queries.Format as Format
import Task
import Util.Result as Result
import Util.Task as Task



-- MAIN ------------------------------------------------------------------------


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


port stdin : (String -> msg) -> Sub msg


type alias Argv =
    List String


main : Program Argv Env Msg
main =
    Platform.worker <|
        let
            init =
                { root = "./"
                , modules = Dict.empty
                }

            actions =
                { read =
                    \path ->
                        Task.do Process.cwd <| \cwd ->
                        Task.do (Path.relative cwd path) <| \relativePath ->
                        let
                            relativePathWithExt =
                                if String.endsWith ".ren" relativePath then
                                    relativePath

                                else
                                    relativePath ++ ".ren"
                        in
                        Fs.readFile relativePathWithExt "utf-8"
                , emit = \_ -> Task.succeed ()
                , eval = \_ -> Task.succeed ()
                }
        in
        { init =
            \argv ->
                ( init
                , Cmd.batch
                    [ Cmd.map (Result.extract Success Failure) <|
                        Query.run actions init <|
                            Query.do (Query.fromTask <| Process.cwd) <| \cwd ->
                            Query.do (Env.update (\env -> { env | root = cwd })) <| \_ ->
                            Query.succeed ()
                    , case argv of
                        "make" :: path :: _ ->
                            Task.perform Basics.identity <| Task.succeed (Command [ "make", path ])

                        "make" :: [] ->
                            Task.perform Basics.identity <| Task.succeed (Command [ "make" ])

                        _ ->
                            Cmd.none
                    ]
                )
        , update =
            \msg env ->
                case msg of
                    Command [ "fmt", path ] ->
                        ( env
                        , Cmd.map (Result.extract Success Failure) <|
                            (Format.mod path
                                |> Query.andThen (\mod -> Query.fromTask <| Fs.writeFile path mod)
                                |> Query.run actions env
                            )
                        )

                    Command [ "make", path ] ->
                        ( env
                        , Cmd.map (Result.extract Success Failure) <|
                            (makePrelude
                                |> Query.andThen (\_ -> makeFile path)
                                |> Query.andThen (\_ -> Query.fromTask <| Process.exit 0)
                                |> Query.run actions env
                            )
                        )

                    Command [ "make" ] ->
                        ( env
                        , Cmd.map (Result.extract Success Failure) <|
                            (makePrelude
                                |> Query.andThen (\_ -> Query.fromTask Process.cwd)
                                |> Query.andThen (\cwd -> makeFile cwd)
                                |> Query.andThen (\_ -> Query.fromTask <| Process.exit 0)
                                |> Query.run actions env
                            )
                        )

                    Command _ ->
                        ( env, Cmd.none )

                    Success ( newEnv, _ ) ->
                        ( newEnv, Cmd.none )

                    Failure ( newEnv, err ) ->
                        ( newEnv, stderr err )
        , subscriptions = \_ -> Sub.none
        }


type alias Env =
    { root : String
    , modules : Dict String Mod
    }


type Msg
    = Command (List String)
    | Failure ( Env, Error )
    | Success ( Env, () )


makePrelude : Query Env ()
makePrelude =
    Query.do Env.get <| \env ->
    Query.doTask (Path.join [ env.root, ".ren", "pkg" ]) <| \pkgs ->
    Query.doTask (Path.join [ pkgs, "prelude.js" ]) <| \prelude ->
    Query.doTask (Fs.writeFile prelude Emit.prelude) <| \_ ->
    Query.succeed ()


makeFile : String -> Query Env ()
makeFile path =
    Query.do Env.get <| \env ->
    Query.do (Query.fromTask <| Path.join [ env.root, ".ren", "pkg" ]) <| \pkgs ->
    Query.doTask (Fs.isDir path) <| \isDir ->
    if isDir then
        Query.doTask (Fs.readDir path) <| \files ->
        Query.do (Query.traverse (\file -> makeFile (path ++ "/" ++ file)) files) <| \_ ->
        Query.succeed ()

    else if String.endsWith ".ren" path then
        Query.try
            (Query.do (Query.fromTask <| Path.dirname path) <| \dir ->
            Query.do (Query.fromTask <| Path.relative dir pkgs) <| \relativePkgPath ->
            Query.do (Query.fromTask <| Path.basename path (Just ".ren")) <| \name ->
            Query.do (Emit.mod relativePkgPath name path) <| \js ->
            Query.do (Env.update (\e -> { e | modules = Dict.remove path e.modules })) <| \_ ->
            Query.do (Query.fromTask <| Fs.writeFile (path ++ ".js") js) <| \_ ->
            Query.succeed ()
            )
            (Query.fromTask << Console.log << List.singleton)
            (\_ -> Query.succeed ())

    else
        Query.succeed ()



-- type alias Argv =
--     List String
-- init : Argv -> FFI.Task ()
-- init argv =
--     case argv of
--         [ "init" ] ->
--             FFI.do Process.cwd <| \cwd ->
--             FFI.do (Path.join [ cwd, "ren.json" ]) <| \configPath ->
--             FFI.do (Fs.exists configPath) <| \exists ->
--             if exists then
--                 Task.fail "ren.json already exists"
--             else
--                 -- Create ren.json
--                 FFI.do (Fs.writeFile configPath <| Project.toJson Project.init) <| \_ ->
--                 -- Create .ren/ dir
--                 FFI.do (Path.join [ cwd, ".ren" ]) <| \renPath ->
--                 FFI.do (Fs.makeDir True renPath) <| \_ ->
--                 FFI.do (Fs.makeDir True <| renPath ++ "/pkg") <| \_ ->
--                 -- Create src/ dir
--                 FFI.do (Path.join [ cwd, "src" ]) <| \srcPath ->
--                 FFI.do (Fs.makeDir True srcPath) <| \_ ->
--                 Task.succeed ()
--         [ "init", "." ] ->
--             FFI.do Process.cwd <| \cwd ->
--             FFI.do (Path.join [ cwd, "ren.json" ]) <| \configPath ->
--             FFI.do (Fs.exists configPath) <| \exists ->
--             if exists then
--                 Task.fail "ren.json already exists"
--             else
--                 -- Create ren.json
--                 FFI.do (Fs.writeFile configPath <| Project.toJson Project.init) <| \_ ->
--                 -- Create .ren/ dir
--                 FFI.do (Path.join [ cwd, ".ren" ]) <| \renPath ->
--                 FFI.do (Fs.makeDir True renPath) <| \_ ->
--                 FFI.do (Fs.makeDir True <| renPath ++ "/pkg") <| \_ ->
--                 -- Create src/ dir
--                 FFI.do (Path.join [ cwd, "src" ]) <| \srcPath ->
--                 FFI.do (Fs.makeDir True srcPath) <| \_ ->
--                 Task.succeed ()
--         [ "init", name ] ->
--             FFI.do Process.cwd <| \cwd ->
--             FFI.do (Path.join [ cwd, name ]) <| \projectPath ->
--             FFI.do (Fs.exists projectPath) <| \exists ->
--             if exists then
--                 Task.fail <| "A directory named " ++ name ++ " already exists."
--             else
--                 FFI.do (Fs.makeDir True projectPath) <| \_ ->
--                 FFI.do (Path.join [ projectPath, "ren.json" ]) <| \configPath ->
--                 FFI.do (Fs.writeFile configPath <| Project.toJson Project.init) <| \_ ->
--                 FFI.do (Path.join [ projectPath, ".ren" ]) <| \renPath ->
--                 FFI.do (Fs.makeDir True renPath) <| \_ ->
--                 FFI.do (Fs.makeDir True <| renPath ++ "/pkg") <| \_ ->
--                 FFI.do (Path.join [ projectPath, "src" ]) <| \srcPath ->
--                 FFI.do (Fs.makeDir True srcPath) <| \_ ->
--                 Task.succeed ()
--         [ "make" ] ->
--             FFI.do Process.cwd <| \cwd ->
--             FFI.do (Path.join [ cwd, "ren.json" ]) <| \configPath ->
--             FFI.do (Fs.exists configPath) <| \exists ->
--             if exists then
--                 FFI.do (Path.join [ cwd, ".ren", "pkg" ]) <| \pkgPath ->
--                 FFI.do (Path.join [ cwd, "src" ]) <| \srcPath ->
--                 -- Write the prelude module.
--                 FFI.do (Fs.writeFile (pkgPath ++ "/prelude.js") Emit.prelude) <| \_ ->
--                 -- Recursively read every `*.ren` file in the project.
--                 FFI.do (readFiles srcPath) <| \files ->
--                 Dict.toList files
--                     |> Task.traverse
--                         (\( filePath, src ) ->
--                             -- Get the file name without the extension.
--                             FFI.do (Path.basename filePath <| Just ".ren") <| \filename ->
--                             -- Get the file path without the file name.
--                             FFI.do (Path.dirname filePath) <| \filedir ->
--                             -- Work out relative path from this module to the
--                             -- root directory for package imports.
--                             FFI.do (Path.relative filedir pkgPath) <| \absPkgPath ->
--                             -- Work out the relative path of this module from the
--                             -- current work directory
--                             FFI.do (Path.relative cwd filePath) <| \relativeFilepath ->
--                             Parse.mod src
--                                 |> Result.map (Mod.transformMeta (Meta.setName filename))
--                                 |> Result.map (Mod.transformMeta (Meta.setPkgPath absPkgPath))
--                                 |> Result.map Emit.mod
--                                 |> Result.map
--                                     (\mod ->
--                                         FFI.do (Console.log [ "✅ Sucessfully compiled " ++ relativeFilepath ]) <| \_ ->
--                                         Fs.writeFile (filePath ++ ".js") mod
--                                     )
--                                 |> Result.mapError
--                                     (\err ->
--                                         FFI.do (Console.log [ "❎ Error compiling " ++ relativeFilepath ]) <| \_ ->
--                                         Task.fail err
--                                     )
--                                 |> Result.unwrap
--                         )
--                     |> Task.replace ()
--             else
--                 Task.fail "ren.json not found, try `ren init` first"
--         _ ->
--             Task.succeed ()
-- readFiles : String -> FFI.Task (Dict String String)
-- readFiles path =
--     FFI.do (Fs.isDir path) <| \isDir ->
--     if isDir then
--         FFI.do (Fs.readDir path) <| \entries ->
--         FFI.do (Task.traverse (\entry -> Path.join [ path, entry ]) entries) <| \paths ->
--         FFI.do (Task.traverse readFiles paths) <| \files ->
--         Task.succeed <| List.foldl Dict.union Dict.empty files
--     else if String.endsWith ".ren" path then
--         FFI.do (Fs.readFile path "utf-8") <| \src ->
--         Task.succeed <| Dict.singleton path src
--     else
--         Task.succeed Dict.empty
-- -- MAIN ------------------------------------------------------------------------
-- main : Program Argv () (Result String ())
-- main =
--     Platform.worker
--         { init =
--             \argv ->
--                 ( (), Task.attempt Basics.identity (init argv) )
--         , update =
--             \result _ ->
--                 case result of
--                     Ok _ ->
--                         ( (), Cmd.none )
--                     Err message ->
--                         ( (), stderr message )
--         , subscriptions = Basics.always Sub.none
--         }
