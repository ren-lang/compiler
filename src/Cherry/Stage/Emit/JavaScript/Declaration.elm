module Cherry.Stage.Emit.JavaScript.Declaration exposing
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JavaScript.Expression as Expression
import Cherry.Stage.Emit.JavaScript.Expression.Pattern as Pattern
import String.Extra


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit: AST.Declaration -> String
emit declaration =
    case declaration of
        AST.Fun visibility name args body bindings ->
            funEmitter name args body bindings
                |> (++) (visibilityEmitter visibility)

        AST.Let visibility name body bindings ->
            letEmitter name body bindings
                |> (++) (visibilityEmitter visibility)


-- FUN EMITTERS ----------------------------------------------------------------


{-| -}
funEmitter : String -> List AST.Pattern -> AST.Expression -> List AST.Binding -> String
funEmitter name args body bindings =
    case ( args, List.length bindings ) of
        ( [], 0 ) ->
            funNoArgsNoBindingsTemplate name body

        ( [], _ ) ->
            funNoArgsTemplate name body bindings

        ( arg :: [], 0 ) ->
            funOneArgNoBindingsTemplate name arg body

        ( arg :: [], _ ) ->
            funOneArgTemplate name arg body bindings

        ( arg :: rest, 0 ) ->
            funManyArgsNoBindingsTemplate name ( arg, rest ) body

        ( arg :: rest, _ ) ->
            funManyArgsTemplate name ( arg, rest ) body bindings


{-| -}
funArgsEmitter : List AST.Pattern -> String
funArgsEmitter args =
    List.map (\arg -> "(" ++ Pattern.emit Expression.emit arg ++ ")") args
        |> String.join " => "


-- FUN TEMPLATES ---------------------------------------------------------------


{-| -}
funNoArgsTemplate : String -> AST.Expression -> List AST.Binding -> String
funNoArgsTemplate name body bindings = String.trim """
function {name} () {
    {bindings}

    return {body}
}
""" |> String.replace "{name}" name
    |> String.replace "{bindings}" (bindingsEmitter bindings |> String.Extra.nest 4 1)
    |> String.replace "{body}" (Expression.emit body)

{-| -}
funNoArgsNoBindingsTemplate : String -> AST.Expression -> String
funNoArgsNoBindingsTemplate name body = String.trim """
function {name} () {
    return {body}
}
""" |> String.replace "{name}" name
    |> String.replace "{body}" (Expression.emit body)

{-| -}
funOneArgTemplate : String -> AST.Pattern -> AST.Expression -> List AST.Binding -> String
funOneArgTemplate name arg body bindings = String.trim """
function {name} ({arg}) {
    {bindings}

    return {body}
}
""" |> String.replace "{name}" name
    |> String.replace "{arg}" (Pattern.emit Expression.emit arg)
    |> String.replace "{bindings}" (bindingsEmitter bindings |> String.Extra.nest 4 1)
    |> String.replace "{body}" (Expression.emit body)

{-| -}
funOneArgNoBindingsTemplate : String -> AST.Pattern -> AST.Expression -> String
funOneArgNoBindingsTemplate name arg body = String.trim """
function {name} ({arg}) {
    return {body}
}
""" |> String.replace "{name}" name
    |> String.replace "{arg}" (Pattern.emit Expression.emit arg)
    |> String.replace "{body}" (Expression.emit body)

{-| -}
funManyArgsTemplate : String -> ( AST.Pattern, List AST.Pattern ) -> AST.Expression -> List AST.Binding -> String
funManyArgsTemplate name ( arg, args ) body bindings = String.trim """
function {name} ({arg}) {
    return {args} => {
        {bindings}

        return {body}
    }
}
""" |> String.replace "{name}" name
    |> String.replace "{arg}" (Pattern.emit Expression.emit arg)
    |> String.replace "{args}" (funArgsEmitter args)
    |> String.replace "{bindings}" (bindingsEmitter bindings |> String.Extra.nest 4 1)
    |> String.replace "{body}" (Expression.emit body)

{-| -}
funManyArgsNoBindingsTemplate : String -> ( AST.Pattern, List AST.Pattern )-> AST.Expression -> String
funManyArgsNoBindingsTemplate name ( arg, args ) body = String.trim """
function {name} ({arg}) {
    return {args} => {
        return {body}
    }
}
""" |> String.replace "{name}" name
    |> String.replace "{arg}" (Pattern.emit Expression.emit arg)
    |> String.replace "{args}" (funArgsEmitter args)
    |> String.replace "{body}" (Expression.emit body)


-- LET EMITTERS ----------------------------------------------------------------


{-| -}
letEmitter : String -> AST.Expression -> List AST.Binding -> String
letEmitter name body bindings =
    case bindings of
        [] ->
            letNoBindingsTemplate name body

        _ ->
            letTemplate name body bindings


-- LET TEMPLATES ---------------------------------------------------------------


{-| -}
letNoBindingsTemplate : String -> AST.Expression -> String
letNoBindingsTemplate name body = String.trim """
var {name} = {body};
""" |> String.replace "{name}" name
    |> String.replace "{body}" (Expression.emit body)

{-| -}
letTemplate : String -> AST.Expression -> List AST.Binding -> String
letTemplate name body bindings = String.trim """
var {name} = (() => {
    {bindings}

    return {body}
})()
""" |> String.replace "{name}" name
    |> String.replace "{bindings}" (bindingsEmitter bindings |> String.Extra.nest 4 1)
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
    "const {name} = {body};"
        |> String.replace "{name}" (Pattern.emit Expression.emit name)
        |> String.replace "{body}" (Expression.emit body)


-- VISIBILITY EMITTER ----------------------------------------------------------


{-| -}
visibilityEmitter : AST.Visibility -> String
visibilityEmitter visibility =
    case visibility of
        AST.Public ->
            "export "

        AST.Private ->
            ""
