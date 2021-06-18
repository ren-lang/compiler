module Ren.Compiler.Emit.CommonJS exposing
    ( fromDeclaration
    , fromExpression
    , fromModule
    )

-- IMPORTS ---------------------------------------------------------------------

import Dict
import Ren.Data.Declaration exposing (Declaration(..))
import Ren.Data.Declaration.Binding exposing (Binding(..))
import Ren.Data.Declaration.Visibility exposing (Visibility(..))
import Ren.Data.Expression as Expression exposing (Expression(..), Identifier)
import Ren.Data.Expression.Accessor exposing (Accessor(..))
import Ren.Data.Expression.Identifier exposing (Identifier(..))
import Ren.Data.Expression.Literal exposing (Literal(..))
import Ren.Data.Expression.Operator exposing (Operator(..))
import Ren.Data.Expression.Pattern exposing (Pattern(..))
import Ren.Data.Module exposing (Module(..))
import Ren.Data.Module.Import exposing (Import(..))
import String.Extra



-- EMITTING MODULES ------------------------------------------------------------


{-| -}
fromModule : Module -> String
fromModule (Module { imports, declarations }) =
    String.trim """
{imports}

{declarations}
"""
        |> String.replace "{imports}" (List.map fromImport imports |> String.join "\n")
        |> String.replace "{declarations}" (List.map fromDeclaration declarations |> String.join "\n")
        |> String.trimLeft


{-| -}
fromImport : Import -> String
fromImport (Import { path, name, exposed }) =
    case ( name, exposed ) of
        ( [], [] ) ->
            importUnqualifiedNoBindingsTemplate path

        ( _, [] ) ->
            importQualifiedNoBindingsTemplate path (String.join "$" name)

        ( [], _ ) ->
            importUnqualifiedTemplate path exposed

        ( _, _ ) ->
            importQualifiedTemplate path (String.join "$" name) exposed


importUnqualifiedTemplate : String -> List String -> String
importUnqualifiedTemplate path bindings =
    String.trim """
const { {bindings} } = require('{path}')
"""
        |> String.replace "{bindings}" (String.join ", " bindings)
        |> String.replace "{path}" path


importUnqualifiedNoBindingsTemplate : String -> String
importUnqualifiedNoBindingsTemplate path =
    String.trim """
require('{path}')
""" |> String.replace "{path}" path


importQualifiedTemplate : String -> String -> List String -> String
importQualifiedTemplate path name bindings =
    String.trim """
const { {bindings}, ...{name} } = require('{path}')
"""
        |> String.replace "{name}" name
        |> String.replace "{bindings}" (String.join ", " bindings)
        |> String.replace "{path}" path


importQualifiedNoBindingsTemplate : String -> String -> String
importQualifiedNoBindingsTemplate path name =
    String.trim """
const {name} = require('{path}')
"""
        |> String.replace "{name}" name
        |> String.replace "{path}" path



-- EMITTING DECLARATIONS -------------------------------------------------------


{-| -}
fromDeclaration : Declaration -> String
fromDeclaration declaration_ =
    case declaration_ of
        Function { visibility, name, args, bindings, body } ->
            fromFunction (fromPattern name) args bindings body
                |> (++) (fromVisibility visibility <| fromPattern name)

        Variable { visibility, name, bindings, body } ->
            fromVariable (fromPattern name) bindings body
                |> (++) (fromVisibility visibility <| fromPattern name)



-- EMIITTING DECLARATIONS: FUNCTIONS -------------------------------------------


{-| -}
fromFunction : String -> List Pattern -> List Declaration -> Expression -> String
fromFunction name args bindings body =
    case ( args, List.length bindings ) of
        ( [], 0 ) ->
            functionNoArgsNoBindingsTemplate name body

        ( [], _ ) ->
            functionNoArgsTemplate name bindings body

        ( arg :: [], 0 ) ->
            functionOneArgNoBindingsTemplate name arg body

        ( arg :: [], _ ) ->
            functionOneArgTemplate name arg bindings body

        ( arg :: rest, 0 ) ->
            functionManyArgsNoBindingsTemplate name ( arg, rest ) body

        ( arg :: rest, _ ) ->
            functionManyArgsTemplate name ( arg, rest ) bindings body


{-| -}
fromFunctionArgs : List Pattern -> String
fromFunctionArgs args =
    List.map (\arg -> "(" ++ fromPattern arg ++ ")") args
        |> String.join " => "


{-| -}
functionNoArgsTemplate : String -> List Declaration -> Expression -> String
functionNoArgsTemplate name bindings body =
    String.trimLeft """
function {name} () {
    {bindings}
    return {body}
}
"""
        |> String.replace "{name}" name
        |> String.replace "{bindings}" (fromBindings bindings |> String.Extra.nest 4 1)
        |> String.replace "{body}" (fromExpression body)


{-| -}
functionNoArgsNoBindingsTemplate : String -> Expression -> String
functionNoArgsNoBindingsTemplate name body =
    String.trimLeft """
function {name} () {
    return {body}
}
"""
        |> String.replace "{name}" name
        |> String.replace "{body}" (fromExpression body)


{-| -}
functionOneArgTemplate : String -> Pattern -> List Declaration -> Expression -> String
functionOneArgTemplate name arg bindings body =
    String.trimLeft """
function {name} ({arg}) {
    {bindings}
    return {body}
}
"""
        |> String.replace "{name}" name
        |> String.replace "{arg}" (fromPattern arg)
        |> String.replace "{bindings}" (fromBindings bindings |> String.Extra.nest 4 1)
        |> String.replace "{body}" (fromExpression body)


{-| -}
functionOneArgNoBindingsTemplate : String -> Pattern -> Expression -> String
functionOneArgNoBindingsTemplate name arg body =
    String.trimLeft """
function {name} ({arg}) {
    return {body}
}
"""
        |> String.replace "{name}" name
        |> String.replace "{arg}" (fromPattern arg)
        |> String.replace "{body}" (fromExpression body)


{-| -}
functionManyArgsTemplate : String -> ( Pattern, List Pattern ) -> List Declaration -> Expression -> String
functionManyArgsTemplate name ( arg, args ) bindings body =
    String.trimLeft """
function {name} ({arg}) {
    return {args} => {
        {bindings}
        return {body}
    }
}
"""
        |> String.replace "{name}" name
        |> String.replace "{arg}" (fromPattern arg)
        |> String.replace "{args}" (fromFunctionArgs args)
        |> String.replace "{bindings}" (fromBindings bindings |> String.Extra.nest 4 2)
        |> String.replace "{body}" (fromExpression body)


{-| -}
functionManyArgsNoBindingsTemplate : String -> ( Pattern, List Pattern ) -> Expression -> String
functionManyArgsNoBindingsTemplate name ( arg, args ) body =
    String.trimLeft """
function {name} ({arg}) {
    return {args} => {
        return {body}
    }
}
"""
        |> String.replace "{name}" name
        |> String.replace "{arg}" (fromPattern arg)
        |> String.replace "{args}" (fromFunctionArgs args)
        |> String.replace "{body}" (fromExpression body)



-- EMIITTING DECLARATIONS: VARIABLES -------------------------------------------


{-| -}
fromVariable : String -> List Declaration -> Expression -> String
fromVariable name bindings body =
    case bindings of
        [] ->
            variableNoBindingsTemplate name body

        _ ->
            variableTemplate name bindings body


{-| -}
variableNoBindingsTemplate : String -> Expression -> String
variableNoBindingsTemplate name body =
    String.trimLeft """
var {name} = {body};
"""
        |> String.replace "{name}" name
        |> String.replace "{body}" (fromExpression body)


{-| -}
variableTemplate : String -> List Declaration -> Expression -> String
variableTemplate name bindings body =
    String.trimLeft """
var {name} = (() => {
    {bindings}
    return {body}
})()
"""
        |> String.replace "{name}" name
        |> String.replace "{bindings}" (fromBindings bindings |> String.Extra.nest 4 1)
        |> String.replace "{body}" (fromExpression body)



-- EMITTING DECLARATIONS: BINDINGS ---------------------------------------------


{-| -}
fromBindings : List Declaration -> String
fromBindings bindings =
    List.map fromDeclaration bindings
        |> String.join ""



-- EMITTING DECLARATIONS: VISIBILITY -------------------------------------------


fromVisibility : Visibility -> String -> String
fromVisibility visibility name =
    case visibility of
        Public ->
            "exports.{name} = {name}\n"
                |> String.replace "{name}" name

        Private ->
            ""



-- EMITTING EXPRESSIONS --------------------------------------------------------


{-| -}
fromExpression : Expression -> String
fromExpression expression =
    case expression of
        Access expr accessors ->
            fromAccess expr accessors

        Application func args ->
            fromApplication func args

        Comment comment ->
            comment

        Conditional predicate true false ->
            fromConditional predicate true false

        Identifier identifier ->
            fromIdentifier identifier

        Infix op lhs rhs ->
            fromInfix op lhs rhs

        Lambda args body ->
            fromLambda args body

        Literal literal ->
            fromLiteral literal

        SubExpression expr ->
            fromSubExpression expr



-- EMITTING EXPRESSIONS: ACCESS ------------------------------------------------


{-| -}
fromAccess : Expression -> List (Accessor Expression) -> String
fromAccess expr accessors =
    "{expr}{accessors}"
        |> String.replace "{expr}" (fromExpression expr)
        |> String.replace "{accessors}" (List.map fromAccessor accessors |> String.join "")


{-| -}
fromAccessor : Accessor Expression -> String
fromAccessor accessor =
    case accessor of
        Computed expr ->
            "[{expr}]"
                |> String.replace "{expr}" (fromExpression expr)

        Fixed key ->
            ".{key}"
                |> String.replace "{key}" key



-- EMITTING EXPRESSIONS: APPLICATION -------------------------------------------


{-| -}
fromApplication : Expression -> List Expression -> String
fromApplication func args =
    "{func} {args}"
        |> String.replace "{func}" (fromExpression func)
        |> String.replace "{args}" (fromApplicationArgs args)


{-| -}
fromApplicationArgs : List Expression -> String
fromApplicationArgs args =
    List.map (\arg -> "(" ++ fromExpression arg ++ ")") args
        |> String.join " "



-- EMITTING EXPRESSIONS: CONDITIONAL -------------------------------------------


{-| -}
fromConditional : Expression -> Expression -> Expression -> String
fromConditional predicate true false =
    "{predicate} ? {true} : {false}"
        |> String.replace "{predicate}" (fromExpression predicate)
        |> String.replace "{true}" (fromExpression true)
        |> String.replace "{false}" (fromExpression false)



-- EMITTING EXPRESSIONS: IDENTIFIER --------------------------------------------


{-| -}
fromIdentifier : Identifier -> String
fromIdentifier identifier =
    case identifier of
        Local name ->
            name

        Scoped namespace name ->
            "{namespace}.{name}"
                |> String.replace "{namespace}" (String.join "$" namespace)
                |> String.replace "{name}" name

        Operator Pipe ->
            "$Function.pipe"

        Operator Compose ->
            "$Function.compose"

        Operator Discard ->
            "$Function.discard"

        Operator Add ->
            "$Math.add"

        Operator Sub ->
            "$Math.sub"

        Operator Mul ->
            "$Math.mul"

        Operator Div ->
            "$Math.div"

        Operator Pow ->
            "$Math.pow"

        Operator Mod ->
            "$Math.mod"

        Operator Eq ->
            "$Compare.eq"

        Operator NotEq ->
            "$Compare.notEq"

        Operator Lt ->
            "$Compare.lt"

        Operator Lte ->
            "$Compare.lte"

        Operator Gt ->
            "$Compare.gt"

        Operator Gte ->
            "$Compare.gte"

        Operator And ->
            "$Logic.and"

        Operator Or ->
            "$Logic.or"

        Operator Cons ->
            "$Array.cons"

        Operator Join ->
            "$Array.join"

        Field fieldName ->
            "$Object.get ('{fieldName}')"
                |> String.replace "{fieldName}" fieldName



-- EMITTING EXPRESSIONS: INFIX -------------------------------------------------


{-| -}
fromInfix : Operator -> Expression -> Expression -> String
fromInfix op lhs rhs =
    case op of
        Pipe ->
            fromApplication rhs [ lhs ]

        Compose ->
            fromApplication
                (Expression.scoped [ "$Function" ] "compose")
                [ lhs, rhs ]

        Discard ->
            "{lhs}, {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Add ->
            "{lhs} + {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Sub ->
            "{lhs} - {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Mul ->
            "{lhs} * {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Div ->
            "{lhs} / {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Pow ->
            "{lhs} ** {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Mod ->
            "{lhs} % {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Eq ->
            "{lhs} == {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        NotEq ->
            "{lhs} != {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Lt ->
            "{lhs} < {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Lte ->
            "{lhs} <= {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Gt ->
            "{lhs} > {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Gte ->
            "{lhs} >= {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        And ->
            "{lhs} && {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Or ->
            "{lhs} || {rhs}"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Cons ->
            "[ {lhs}, ...{rhs} ]"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)

        Join ->
            "[ ...{lhs}, ...{rhs} ]"
                |> String.replace "{lhs}" (fromExpression lhs)
                |> String.replace "{rhs}" (fromExpression rhs)



-- EMITTING EXPRESSIONS: LAMBDA ------------------------------------------------


{-| -}
fromLambda : List Pattern -> Expression -> String
fromLambda args body =
    "{args} => {body}"
        |> String.replace "{args}" (fromLambdaArgs args)
        |> String.replace "{body}" (fromExpression body)


{-| -}
fromLambdaArgs : List Pattern -> String
fromLambdaArgs args =
    List.map (\arg -> "(" ++ fromPattern arg ++ ")") args
        |> String.join " => "


{-| -}
fromPattern : Pattern -> String
fromPattern pattern =
    case pattern of
        ArrayDestructure patterns ->
            fromArrayDestructure patterns

        Name name ->
            name

        ObjectDestructure patterns ->
            fromObjectDestructure patterns

        Value literal ->
            fromPrimitiveLiteral literal

        Wildcard name ->
            "_{name}"
                |> String.replace "{name}" (Maybe.withDefault "" name)


{-| -}
fromArrayDestructure : List Pattern -> String
fromArrayDestructure patterns =
    "[ {patterns} ]"
        |> String.replace "{patterns}" (List.map fromPattern patterns |> String.join ", ")


{-| -}
fromObjectDestructure : List ( String, Maybe Pattern ) -> String
fromObjectDestructure patterns =
    "{ {patterns} }"
        |> String.replace "{patterns}" (List.map fromObjectDestructurePattern patterns |> String.join ", ")


{-| -}
fromObjectDestructurePattern : ( String, Maybe Pattern ) -> String
fromObjectDestructurePattern pattern =
    case pattern of
        ( key, Just nestedPattern ) ->
            "{key}: {pattern}"
                |> String.replace "{key}" key
                |> String.replace "{pattern}" (fromPattern nestedPattern)

        ( key, Nothing ) ->
            key



-- EMITTING EXPRESSIONS: LITERAL -----------------------------------------------


{-| -}
fromLiteral : Literal Expression -> String
fromLiteral literal =
    case literal of
        Array elements ->
            fromArray elements

        Boolean bool ->
            if bool then
                "true"

            else
                "false"

        Number n ->
            String.fromFloat n

        Object fields ->
            fromObject (Dict.toList fields)

        String s ->
            "'" ++ s ++ "'"

        Undefined ->
            "undefined"


{-| -}
fromPrimitiveLiteral : Literal Never -> String
fromPrimitiveLiteral literal =
    case literal of
        Array _ ->
            ""

        Boolean bool ->
            if bool then
                "true"

            else
                "false"

        Number n ->
            String.fromFloat n

        Object _ ->
            ""

        String s ->
            "'" ++ s ++ "'"

        Undefined ->
            "undefined"


{-| -}
fromArray : List Expression -> String
fromArray elements =
    "[ {elements} ]"
        |> String.replace "{elements}" (List.map fromExpression elements |> String.join ", ")


{-| -}
fromObject : List ( String, Expression ) -> String
fromObject entries =
    "{ {entries} }"
        |> String.replace "{entries}" (List.map fromObjectField entries |> String.join ", ")


{-| -}
fromObjectField : ( String, Expression ) -> String
fromObjectField ( key, val ) =
    "{key}: {val}"
        |> String.replace "{key}" key
        |> String.replace "{val}" (fromExpression val)



-- EMITTING EXPRESSIONS: SUBEXPRESSION -----------------------------------------------


{-| -}
fromSubExpression : Expression -> String
fromSubExpression expression =
    "({expr})"
        |> String.replace "{expr}" (fromExpression expression)
