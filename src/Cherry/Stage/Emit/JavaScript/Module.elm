module Cherry.Stage.Emit.JavaScript.Module exposing 
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JavaScript.Declaration as Declaration


-- EMITTERS --------------------------------------------------------------------


{-| -}
emit: AST.Module -> String
emit { imports, declarations } =
    String.join "\n\n"
        [ List.map importEmitter imports 
            |> String.join "\n"
        , List.map Declaration.emit declarations
            |> String.join "\n\n"
        ]


{-| -}
importEmitter : AST.Import -> String
importEmitter { path, name, exposedBindings } =
    let
        fullName = String.join "$" name
        bindings = String.join ", " exposedBindings
    in
    case ( name, exposedBindings ) of
        ( [], [] ) ->
            "require('{path}');"
                |> String.replace "{path}" path

        ( _, [] ) ->
            "const {name} = require('{path}');"
                |> String.replace "{name}" fullName
                |> String.replace "{path}" path
        
        ( [], _ ) ->
            "const { {bindings} } = require('{path}');"
                |> String.replace "{bindings}" bindings
                |> String.replace "{path}" path
        
        ( _, _ ) ->
            "const { {bindings}, ...{name} } = require('{path}');"
                |> String.replace "{name}" fullName
                |> String.replace "{bindings}" bindings
                |> String.replace "{path}" path
