module Cherry.Stage.Generate exposing 
    ( Format(..), generate
    , generateCherry, generateJavaScript, generateJSON
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Generate.Cherry as Cherry
import Cherry.Stage.Generate.JavaScript as JavaScript
import Cherry.Stage.Generate.JSON.Module as JSON


-- TYPES -----------------------------------------------------------------------


{-| -}
type Format
    = Cherry
    | JavaScript
    | JSON


-- RUNNING THE GENERATORS ------------------------------------------------------


{-| -}
generate : Format -> AST.Module -> String
generate format ast =
    case format of
        Cherry ->
            generateCherry ast

        JavaScript ->
            generateJavaScript ast

        JSON ->
            generateJSON ast


{-| -}
generateCherry : AST.Module -> String
generateCherry ast =
    Cherry.run ast

{-| -}
generateJavaScript : AST.Module -> String
generateJavaScript ast =
    JavaScript.run ast

{-| -}
generateJSON : AST.Module -> String
generateJSON ast =
    JSON.run ast
