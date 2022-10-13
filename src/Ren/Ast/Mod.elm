module Ren.Ast.Mod exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Ast.Decl as Decl exposing (Decl)
import Ren.Ast.Decl.Ext as Ext exposing (Ext(..))
import Ren.Ast.Decl.Imp as Imp exposing (Imp(..))
import Ren.Ast.Decl.Let as Let exposing (Let(..))
import Ren.Ast.Decl.Type as Type exposing (Type)
import Ren.Compiler.FFI exposing (Error)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Data.Span as Span exposing (Span)
import Util.List as List
import Util.Maybe as Maybe



-- TYPES -----------------------------------------------------------------------


type alias Mod =
    { path : String

    --
    , imports : Dict String Imp
    , types : Dict String Type
    , externals : Dict String Ext
    , bindings : Dict String Let
    , comments : List ( Span, List String )
    }



-- CONSTRUCTORS ----------------------------------------------------------------


new : String -> List Decl -> Mod
new path decls =
    let
        empty =
            { path = path
            , imports = Dict.empty
            , types = Dict.empty
            , externals = Dict.empty
            , bindings = Dict.empty
            , comments = []
            }
    in
    List.foldr
        (\decl mod ->
            case decl of
                Decl.Let binding ->
                    { mod | bindings = Dict.insert (Let.name binding) binding mod.bindings }

                Decl.Ext external ->
                    { mod | externals = Dict.insert (Ext.name external) external mod.externals }

                Decl.Imp imp ->
                    { mod | imports = Dict.insert (Imp.path imp) imp mod.imports }

                Decl.Type typ ->
                    { mod | types = Dict.insert (Type.name typ) typ mod.types }

                Decl.Comment span comment ->
                    { mod | comments = ( span, comment ) :: mod.comments }
        )
        empty
        decls



-- QUERIES ---------------------------------------------------------------------


lookup : String -> Mod -> Maybe Decl
lookup nameOrPath mod =
    List.foldl Maybe.or Nothing <|
        [ Maybe.map Decl.Imp <| Dict.get nameOrPath mod.imports
        , Maybe.map Decl.Type <| Dict.get nameOrPath mod.types
        , Maybe.map Decl.Ext <| Dict.get nameOrPath mod.externals
        , Maybe.map Decl.Let <| Dict.get nameOrPath mod.bindings
        ]


emittables : Mod -> List (List Decl)
emittables mod =
    [ List.map Decl.Imp <| Dict.values mod.imports
    , List.sortWith (\a b -> Span.compare (Decl.span a) (Decl.span b)) <|
        List.concat
            [ List.map Decl.Let <| Dict.values mod.bindings
            , List.map Decl.Ext <| Dict.values mod.externals
            ]
    ]


declarations : Mod -> List Decl
declarations mod =
    List.sortWith (\a b -> Span.compare (Decl.span a) (Decl.span b)) <|
        List.concat
            [ List.map Decl.Imp <| Dict.values mod.imports
            , List.map Decl.Type <| Dict.values mod.types
            , List.map Decl.Let <| Dict.values mod.bindings
            , List.map Decl.Ext <| Dict.values mod.externals
            , List.map (\( span, comment ) -> Decl.Comment span comment) mod.comments
            ]



-- MANIPULATIONS ---------------------------------------------------------------


updateBinding : String -> (Let -> Let) -> Mod -> Mod
updateBinding name f mod =
    { mod | bindings = Dict.update name (Maybe.map f) mod.bindings }



-- PARSING ---------------------------------------------------------------------


parser : String -> Parser () Error Mod
parser path =
    Parser.succeed (new path)
        |> Parser.keep (Parser.many Decl.parser)
        |> Parser.drop (Parser.end "")
