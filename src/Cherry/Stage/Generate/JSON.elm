module Cherry.Stage.Generate.JSON exposing 
    ( generateModule
    , generateDeclaration
    , generateExpression
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Generate.JSON.Module as Module
import Cherry.Stage.Generate.JSON.Declaration as Declaration
import Cherry.Stage.Generate.JSON.Expression as Expression


-- RUNNING THE GENERATORS ------------------------------------------------------


{-| -}
generateModule : AST.Module -> String
generateModule ast =
    Module.run ast

{-| -}
generateDeclaration : AST.Declaration -> String
generateDeclaration ast =
    Declaration.run ast

{-| -}
generateExpression : AST.Expression -> String
generateExpression ast =
    Expression.run ast
