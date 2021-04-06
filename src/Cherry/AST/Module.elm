module Cherry.AST.Module exposing 
    ( Module
    , Import(..)
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Declaration exposing (Declaration)


-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Module =
    { imports : List Import
    , declarations : List Declaration
    }

{-| -}
type Import
    = Import String (Maybe (List String)) (Maybe (List String))
