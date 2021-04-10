module Cherry.Stage.Emit.JavaScript.Declaration exposing
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JavaScript.Expression as Expression
import Cherry.Stage.Emit.JavaScript.Expression.Pattern as Pattern


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit: AST.Declaration -> String
emit declaration =
    case declaration of
        AST.Fun visibility name args body bindings ->
            funEmitter name args body bindings
                |> (++) (visibilityEmitter visibility name)

        AST.Let visibility name body bindings ->
            letEmitter name body bindings
                |> (++) (visibilityEmitter visibility name)

{-| -}
funEmitter : String -> List AST.Pattern -> AST.Expression -> List AST.Binding -> String
funEmitter name args body bindings =
    case args of
        [] ->
            """function {name} () { 
                {bindings} 

                return {body};
            };"""
                |> String.replace "{name}" name
                |> String.replace "{bindings}" (bindingsEmitter bindings)
                |> String.replace "{body}" (Expression.emit body)            

        arg :: [] ->
            """function {name} ({arg}) { 
                {bindings} 

                return {body};
            };"""
                |> String.replace "{name}" name
                |> String.replace "{arg}" (Pattern.emit Expression.emit arg)
                |> String.replace "{bindings}" (bindingsEmitter bindings)
                |> String.replace "{body}" (Expression.emit body)

        arg :: rest ->
            """function {name} ({arg}) {
                return {args} => {
                    {bindings} 

                    return {body};
                }
            };"""
                |> String.replace "{name}" name
                |> String.replace "{arg}" (Pattern.emit Expression.emit arg)
                |> String.replace "{args}" (funArgsEmitter rest)
                |> String.replace "{bindings}" (bindingsEmitter bindings)
                |> String.replace "{body}" (Expression.emit body)

{-| -}
funArgsEmitter : List AST.Pattern -> String
funArgsEmitter args =
    List.map (\arg -> "(" ++ Pattern.emit Expression.emit arg ++ ")") args
        |> String.join " => "


-- LET EMITTERS ----------------------------------------------------------------


{-| -}
letEmitter : String -> AST.Expression -> List AST.Binding -> String
letEmitter name body bindings =
    case bindings of
        [] ->
            "var {name} = {body};"
                |> String.replace "{name}" name
                |> String.replace "{body}" (Expression.emit body)

        _ ->
            """var {name} = (() => {
                {bindings}

                return {body};
            })();"""
                |> String.replace "{name}" name
                |> String.replace "{bindings}" (bindingsEmitter bindings)
                |> String.replace "{body}" (Expression.emit body)



-- BINDINGS EMITTERS -----------------------------------------------------------


{-| -}
bindingsEmitter : List AST.Binding -> String
bindingsEmitter bindings =
    List.map bindingEmitter bindings
        |> String.join "\n"

{-| -}
bindingEmitter : AST.Binding -> String
bindingEmitter { name, body } =
    "var {name} = {body};"
        |> String.replace "{name}" name
        |> String.replace "{body}" (Expression.emit body)


-- VISIBILITY EMITTER ----------------------------------------------------------


{-| -}
visibilityEmitter : AST.Visibility -> String -> String
visibilityEmitter visibility name =
    case visibility of
        AST.Public ->
            "exports.{name} = {name};\n"
                |> String.replace "{name}" name

        AST.Private ->
            ""
