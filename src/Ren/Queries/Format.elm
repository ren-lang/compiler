module Ren.Queries.Format exposing (..)

{-|

@docs mod

-}

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Pretty
import Ren.Ast.Decl as Decl exposing (Decl)
import Ren.Ast.Decl.Ext exposing (Ext(..))
import Ren.Ast.Decl.Imp as Imp exposing (Imp(..))
import Ren.Ast.Decl.Let exposing (Let(..))
import Ren.Ast.Decl.Type exposing (Type(..))
import Ren.Ast.Expr exposing (Expr)
import Ren.Ast.Mod as Mod exposing (Mod)
import Ren.Ast.Type as Type
import Ren.Control.Query as Query exposing (Query)
import Ren.Control.Query.Env as Env
import Ren.Queries.Parse as Parse
import Util.Pretty as Pretty



--


mod : String -> Query { env | modules : Dict String Mod } String
mod path =
    Query.do (Parse.file path) <| \m ->
    Query.do (Env.update (\env -> { env | modules = Dict.remove path env.modules })) <| \_ ->
    Query.succeed <| Pretty.pretty 80 <| prettyMod m



-- PRETTY PRINTING -------------------------------------------------------------


prettyMod : Mod -> Pretty.Doc ()
prettyMod m =
    Mod.declarations m
        |> List.map prettyDecl
        |> Pretty.join Pretty.line


prettyDecl : Decl -> Pretty.Doc ()
prettyDecl decl =
    case decl of
        Decl.Ext (Ext { docs } pub name annotation body) ->
            Pretty.when (Basics.not <| List.isEmpty docs)
                (List.map Pretty.string docs
                    |> Pretty.join Pretty.line
                    |> Pretty.a Pretty.line
                )
                |> Pretty.a (Pretty.when pub (Pretty.string "pub" |> Pretty.a Pretty.space))
                |> Pretty.a (Pretty.string "ext")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string name)
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (Pretty.maybe annotation
                        (\t ->
                            Pretty.string ":"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.string <| Type.toString t)
                                |> Pretty.a Pretty.space
                        )
                    )
                |> Pretty.a (Pretty.char '=')
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.surround (Pretty.char '"') (Pretty.char '"') (Pretty.string body))
                |> Pretty.a Pretty.line

        Decl.Imp (Imp _ path source name) ->
            Pretty.string "import"
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (case source of
                        Imp.External ->
                            Pretty.string "ext"
                                |> Pretty.a Pretty.space

                        Imp.Package ->
                            Pretty.string "pkg"
                                |> Pretty.a Pretty.space

                        Imp.Relative ->
                            Pretty.empty
                    )
                |> Pretty.a (Pretty.surround (Pretty.char '"') (Pretty.char '"') <| Pretty.string path)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "as")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.join (Pretty.char '.') <| List.map Pretty.string name)

        Decl.Let (Let { docs } pub name annotation body) ->
            Pretty.when (Basics.not <| List.isEmpty docs)
                (List.map Pretty.string docs
                    |> Pretty.join Pretty.line
                    |> Pretty.a Pretty.line
                )
                |> Pretty.a (Pretty.when pub (Pretty.string "pub" |> Pretty.a Pretty.space))
                |> Pretty.a (Pretty.string "let")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string name)
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (Pretty.maybe annotation
                        (\t ->
                            Pretty.string ":"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.string <| Type.toString t)
                                |> Pretty.a Pretty.space
                        )
                    )
                |> Pretty.a (Pretty.char '=')
                |> Pretty.a Pretty.space
                |> Pretty.a (prettyExpr body)
                |> Pretty.a Pretty.line

        Decl.Type (Type _ _ _ _ _) ->
            Pretty.string "# TODO: format type declarations"
                |> Pretty.a Pretty.line

        Decl.Comment _ lines ->
            List.map Pretty.string lines
                |> Pretty.join Pretty.line


prettyExpr : Expr -> Pretty.Doc ()
prettyExpr expr =
    Pretty.string "# TODO: format expressions"
