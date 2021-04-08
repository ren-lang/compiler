module Cherry.Stage.Parse exposing
    ( parseModule
    , parseDeclaration
    , parseExpression
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Parse.Expression as Expression
import Cherry.Stage.Parse.Declaration as Declaration
import Cherry.Stage.Parse.Module as Module
import Parser


-- RUNNING THE PARSERS ---------------------------------------------------------


{-| -}
parseModule : String -> Result (List Parser.DeadEnd) AST.Module
parseModule input =
    Module.run input

{-| -}
parseDeclaration : String -> Result (List Parser.DeadEnd) AST.Declaration
parseDeclaration input =
    Declaration.run input

{-| -}
parseExpression : String -> Result (List Parser.DeadEnd) AST.Expression
parseExpression input =
    Expression.run input
