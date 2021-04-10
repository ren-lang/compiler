module Cherry.Stage.Emit.JSON exposing 
    ( emitModule
    , emitDeclaration
    , emitExpression
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JSON.Module as Module
import Cherry.Stage.Emit.JSON.Declaration as Declaration
import Cherry.Stage.Emit.JSON.Expression as Expression
import Json.Encode


-- RUNNING THE EMITTERS ------------------------------------------------------


{-| -}
emitModule : AST.Module -> String
emitModule ast =
    Module.emit ast
        |> Json.Encode.encode 4

{-| -}
emitDeclaration : AST.Declaration -> String
emitDeclaration ast =
    Declaration.emit ast
        |> Json.Encode.encode 4

{-| -}
emitExpression : AST.Expression -> String
emitExpression ast =
    Expression.emit ast
        |> Json.Encode.encode 4
