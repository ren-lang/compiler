module Ren.Queries.Parse exposing (file)

{-|

@docs file

-}

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Ast.Mod as Mod exposing (Mod)
import Ren.Control.Lexer as Lexer
import Ren.Control.Parser as Parser
import Ren.Control.Query as Query exposing (Query)
import Ren.Control.Query.Env as Env



-- QUERIES ---------------------------------------------------------------------


{-| -}
file : String -> Query { env | modules : Dict String Mod } Mod
file path =
    Query.do Env.get <| \env ->
    case Dict.get path env.modules of
        Just m ->
            Query.succeed m

        Nothing ->
            Query.do (Query.read path) <| \contents ->
            Query.do (Query.fromResult <| Lexer.run contents) <| \tokens ->
            Query.do (Query.fromResult <| Parser.run (Mod.parser path) tokens) <| \m ->
            Query.do (Env.set { env | modules = Dict.insert path m env.modules }) <| \_ ->
            Query.succeed m
