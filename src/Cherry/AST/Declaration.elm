module Cherry.AST.Declaration exposing 
    ( Declaration(..)
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Expression as Expression exposing (Expression)


-- TYPES -----------------------------------------------------------------------


{-| -}
type Declaration
    = Function Bool String (List Expression.Variable) Expression (List ( String, Expression ))
    | Variable Bool String Expression (List ( String, Expression ))
