module Cherry.Stage.Emit.JavaScript exposing 
    ( emitModule
    , emitDeclaration
    , emitExpression
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JavaScript.Module as Module
import Cherry.Stage.Emit.JavaScript.Declaration as Declaration
import Cherry.Stage.Emit.JavaScript.Expression as Expression


-- RUNNING THE EMITTERS ------------------------------------------------------


{-| -}
emitModule : AST.Module -> String
emitModule ast =
    Module.emit ast

{-| -}
emitDeclaration : AST.Declaration -> String
emitDeclaration ast =
    Declaration.emit ast

{-| -}
emitExpression : AST.Expression -> String
emitExpression ast =
    Expression.emit ast