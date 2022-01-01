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



-- MANIPULATIONS ---------------------------------------------------------------


map : (Declaration a -> Declaration b) -> Module a -> Module b
map f { imports, declarations } =
    { imports = imports
    , declarations = List.map f declarations
    }
