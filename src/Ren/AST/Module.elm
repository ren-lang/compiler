module Ren.AST.Module exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.AST.Expr exposing (Expr)
import Ren.Data.Type exposing (Type)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Module meta =
    { imports : List Import
    , declarations : List (Declaration meta)
    }


{-| -}
type alias Import =
    { path : String
    , name : List String
    , exposed : List String
    }


{-| -}
type alias Declaration meta =
    { public : Bool
    , name : String
    , type_ : Type
    , expr : Expr meta
    , meta : meta
    }



-- QUERIES ---------------------------------------------------------------------


{-| -}
exposes : String -> Module meta -> Bool
exposes n m =
    List.any (\{ public, name } -> public && name == n) m.declarations


{-| -}
imports : String -> Module meta -> Bool
imports p m =
    List.any (\{ path } -> path == p) m.imports



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
map : (Declaration a -> Declaration b) -> Module a -> Module b
map f m =
    { imports = m.imports
    , declarations = List.map f m.declarations
    }
