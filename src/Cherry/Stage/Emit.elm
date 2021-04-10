module Cherry.Stage.Emit exposing 
    ( Format(..), emit
    , emitJavaScript, emitJSON
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JavaScript as JavaScript
import Cherry.Stage.Emit.JSON as JSON


-- TYPES -----------------------------------------------------------------------


{-| -}
type Format
    = JavaScript
    | JSON


-- RUNNING THE EMITTERS ------------------------------------------------------


{-| -}
emit : Format -> AST.Module -> String
emit format ast =
    case format of
        JavaScript ->
            emitJavaScript ast

        JSON ->
            emitJSON ast


{-| -}
emitJavaScript : AST.Module -> String
emitJavaScript ast =
    JavaScript.emitModule ast

{-| -}
emitJSON : AST.Module -> String
emitJSON ast =
    JSON.emitModule ast
