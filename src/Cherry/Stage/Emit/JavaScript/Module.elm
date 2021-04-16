module Cherry.Stage.Emit.JavaScript.Module exposing 
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JavaScript.Declaration as Declaration


-- EMITTERS --------------------------------------------------------------------


{-| -}
emit: AST.Module -> String
emit { imports, declarations } = String.trim """
{defaultImports}
{imports}

{declarations}
""" |> String.replace "{defaultImports}" defaultImports
    |> String.replace "{imports}" (List.map importEmitter imports |> String.join "\n")
    |> String.replace "{declarations}" (List.map Declaration.emit declarations |> String.join "\n\n")

defaultImports : String
defaultImports = String.trim """
import * as $Array from './.cherry/array.js'
import * as $Compare from './.cherry/compare.js'
import * as $Function from './.cherry/function.js'
import * as $Logic from './.cherry/logic.js'
import * as $Math from './.cherry/math.js'
import * as $Object from './.cherry/object.js'
"""


{-| -}
importEmitter : AST.Import -> String
importEmitter { path, name, exposedBindings } =
    let
        fullName = String.join "$" name
    in
    case ( name, exposedBindings ) of
        ( [], [] ) ->
            importUnqualifiedNoBindingsTemplate path

        ( _, [] ) ->
            importQualifiedNoBindings path fullName
        
        ( [], _ ) ->
            importUnqualified path exposedBindings
        
        ( _, _ ) ->
            importQualified path fullName exposedBindings


-- IMPORT TEMPLATES ------------------------------------------------------------


importUnqualified : String -> List String -> String
importUnqualified path bindings = String.trim """
import { {bindings} } from '{path}'
""" |> String.replace "{bindings}" (String.join ", " bindings)
    |> String.replace "{path}" path

importUnqualifiedNoBindingsTemplate : String -> String
importUnqualifiedNoBindingsTemplate path = String.trim """
import '{path}'
""" |> String.replace "{path}" path

importQualified : String -> String -> List String -> String
importQualified path name bindings = String.trim """
import * as {name} from '{path}'
import { {bindings} } from '{path}'
""" |> String.replace "{name}" name
    |> String.replace "{bindings}" (String.join ", " bindings)
    |> String.replace "{path}" path

importQualifiedNoBindings : String -> String -> String
importQualifiedNoBindings path name = String.trim """
import * as {name} from '{path}'
""" |> String.replace "{name}" name
    |> String.replace "{path}" path