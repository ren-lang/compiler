module Ren.AST.Module exposing
    ( Module, Import, ImportSpecifier(..), Declaration(..)
    , exposes, imports, externs
    , map, mapImports
    )

{-|

@docs Module, Import, ImportSpecifier, Declaration
@docs exposes, imports, externs
@docs map, mapImports

-}

-- IMPORTS ---------------------------------------------------------------------

import Ren.AST.Expr exposing (Expr)
import Ren.Data.Type exposing (Type)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Module meta =
    { name : String
    , imports : List Import
    , declarations : List (Declaration meta)
    }


{-| -}
type alias Import =
    { path : ImportSpecifier
    , name : List String
    , exposed : List String
    }


{-| -}
type ImportSpecifier
    = ExternalImport String
    | LocalImport String
    | PackageImport String
    | FfiImport


{-| -}
type Declaration meta
    = Ext Bool String Type meta
    | Let Bool String Type (Expr meta) meta
    | Run (Expr meta) meta



-- QUERIES ---------------------------------------------------------------------


{-| -}
exposes : String -> Module meta -> Bool
exposes n m =
    List.any
        (\declr ->
            case declr of
                Ext True name _ _ ->
                    name == n

                Let True name _ _ _ ->
                    name == n

                _ ->
                    False
        )
        m.declarations


{-| -}
imports : ImportSpecifier -> Module meta -> Bool
imports p m =
    List.any (\{ path } -> path == p) m.imports


{-| -}
externs : Module meta -> List String
externs m =
    List.filterMap
        (\declr ->
            case declr of
                Ext _ name _ _ ->
                    Just name

                _ ->
                    Nothing
        )
        m.declarations



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
map : (Declaration a -> Declaration b) -> Module a -> Module b
map f m =
    { name = m.name
    , imports = m.imports
    , declarations = List.map f m.declarations
    }


{-| -}
mapImports : (Import -> Import) -> Module meta -> Module meta
mapImports f m =
    { m | imports = List.map f m.imports }
