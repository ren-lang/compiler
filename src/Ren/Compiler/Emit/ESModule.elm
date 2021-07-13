module Ren.Compiler.Emit.ESModule exposing
    ( emitDeclaration
    , emitExpression
    , emitModule
    )

-- IMPORTS ---------------------------------------------------------------------

import Dict
import Pretty
import Ren.Data.Declaration exposing (Declaration(..))
import Ren.Data.Declaration.Visibility exposing (Visibility(..))
import Ren.Data.Expression as Expression exposing (Expression(..))
import Ren.Data.Expression.Accessor exposing (Accessor(..))
import Ren.Data.Expression.Identifier exposing (Identifier(..))
import Ren.Data.Expression.Literal exposing (Literal(..))
import Ren.Data.Expression.Operator exposing (Operator(..))
import Ren.Data.Expression.Pattern exposing (Pattern(..))
import Ren.Data.Module exposing (Module(..))
import Ren.Data.Module.Import exposing (Import(..))



-- EMITTING MODULES ------------------------------------------------------------


{-| -}
emitModule : Module -> String
emitModule =
    fromModule >> Pretty.pretty 80


fromModule : Module -> Pretty.Doc t
fromModule (Module { imports, declarations }) =
    Pretty.lines (List.map fromImport imports)
        |> Pretty.a Pretty.line
        |> Pretty.a Pretty.line
        |> Pretty.a
            (declarations
                |> List.map (fromDeclaration >> Pretty.a Pretty.line)
                |> Pretty.lines
            )


fromImport : Import -> Pretty.Doc t
fromImport (Import ({ path, name, exposed } as import_)) =
    case ( List.isEmpty name, List.isEmpty exposed ) of
        ( True, True ) ->
            Pretty.string "import"
                |> Pretty.a Pretty.space
                |> Pretty.a (quotes path)

        ( True, False ) ->
            Pretty.string "import"
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (List.map Pretty.string exposed
                        |> Pretty.join (Pretty.char ',' |> Pretty.a Pretty.space)
                        |> Pretty.braces
                    )
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "from")
                |> Pretty.a Pretty.space
                |> Pretty.a (quotes path)

        ( False, True ) ->
            Pretty.string "import"
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '*')
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "as")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string <| String.join "$" name)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "from")
                |> Pretty.a Pretty.space
                |> Pretty.a (quotes path)

        ( False, False ) ->
            Pretty.lines
                [ fromImport (Import { import_ | exposed = [] })
                , fromImport (Import { import_ | name = [] })
                ]



-- EMITTING DECLARATIONS -------------------------------------------------------


{-| -}
emitDeclaration : Declaration -> String
emitDeclaration =
    fromDeclaration >> Pretty.pretty 80


fromDeclaration : Declaration -> Pretty.Doc t
fromDeclaration declaration =
    case declaration of
        Function { visibility, name, args, bindings, body } ->
            fromVisibility visibility
                |> Pretty.a (fromFunction name args bindings body)

        Variable { visibility, name, bindings, body } ->
            fromVisibility visibility
                |> Pretty.a (fromVariable name bindings body)



-- EMIITTING DECLARATIONS: FUNCTIONS -------------------------------------------


fromFunction : Pattern -> List Pattern -> List Declaration -> Expression -> Pretty.Doc t
fromFunction name args bindings body =
    case args of
        -- Ren doesn't allow nullary function definitions, so this case should
        -- never be hit.
        [] ->
            Pretty.empty

        arg :: [] ->
            Pretty.string "function"
                |> Pretty.a Pretty.space
                |> Pretty.a (fromPattern name)
                |> Pretty.a Pretty.space
                |> Pretty.a (fromPattern arg |> Pretty.parens)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    ((if List.isEmpty bindings then
                        Pretty.empty

                      else
                        List.map fromDeclaration bindings
                            |> Pretty.lines
                            |> Pretty.a Pretty.line
                            |> Pretty.a Pretty.line
                     )
                        |> Pretty.a (Pretty.string "return")
                        |> Pretty.a Pretty.space
                        |> Pretty.a (fromExpression body)
                        |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        arg :: rest ->
            Pretty.string "function"
                |> Pretty.a Pretty.space
                |> Pretty.a (fromPattern name)
                |> Pretty.a Pretty.space
                |> Pretty.a (fromPattern arg |> Pretty.parens)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (Pretty.string "return"
                        |> Pretty.a Pretty.space
                        |> Pretty.a
                            (List.map (fromPattern >> Pretty.parens) rest
                                |> Pretty.join (Pretty.space |> Pretty.a (Pretty.string "=>") |> Pretty.a Pretty.space)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.string "=>")
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    ((if List.isEmpty bindings then
                                        Pretty.empty

                                      else
                                        List.map fromDeclaration bindings
                                            |> Pretty.lines
                                            |> Pretty.a Pretty.line
                                            |> Pretty.a Pretty.line
                                     )
                                        |> Pretty.a (Pretty.string "return")
                                        |> Pretty.a Pretty.space
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                            )
                        |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')



-- EMIITTING DECLARATIONS: VARIABLES -------------------------------------------


fromVariable : Pattern -> List Declaration -> Expression -> Pretty.Doc t
fromVariable name bindings body =
    if List.isEmpty bindings then
        Pretty.string "var"
            |> Pretty.a Pretty.space
            |> Pretty.a (fromPattern name)
            |> Pretty.a Pretty.space
            |> Pretty.a (Pretty.char '=')
            |> Pretty.a Pretty.space
            |> Pretty.a (fromExpression body)

    else
        Pretty.string "var"
            |> Pretty.a Pretty.space
            |> Pretty.a (fromPattern name)
            |> Pretty.a Pretty.space
            |> Pretty.a (Pretty.char '=')
            |> Pretty.a Pretty.space
            |> Pretty.a (Pretty.char '{')
            |> Pretty.a Pretty.line
            |> Pretty.a
                (List.map fromDeclaration bindings
                    |> Pretty.lines
                    |> Pretty.a Pretty.line
                    |> Pretty.a Pretty.line
                    |> Pretty.a (Pretty.string "return")
                    |> Pretty.a Pretty.space
                    |> Pretty.a (fromExpression body)
                    |> Pretty.indent 4
                )
            |> Pretty.a Pretty.line
            |> Pretty.a (Pretty.char '}')



-- EMITTING DECLARATIONS: VISIBILITY -------------------------------------------


fromVisibility : Visibility -> Pretty.Doc t
fromVisibility visibility =
    case visibility of
        Public ->
            Pretty.string "export"
                |> Pretty.a Pretty.space

        Private ->
            Pretty.empty



-- EMITTING EXPRESSIONS --------------------------------------------------------


emitExpression : Expression -> String
emitExpression =
    fromExpression >> Pretty.pretty 80


fromExpression : Expression -> Pretty.Doc t
fromExpression expression =
    case expression of
        Access expr accessors ->
            fromAccess expr accessors

        Application expr args ->
            fromApplication expr args

        Conditional condition true false ->
            fromConditional condition true false

        Identifier identifier ->
            fromIdentifier identifier

        Infix operator lhs rhs ->
            fromInfix operator lhs rhs

        Lambda args body ->
            fromLambda args body

        Literal literal ->
            fromLiteral literal

        Match expr cases ->
            fromMatch expr cases

        SubExpression expr ->
            fromSubexpression expr



-- EMITTING EXPRESSIONS: ACCESS ------------------------------------------------


fromAccess : Expression -> List (Accessor Expression) -> Pretty.Doc t
fromAccess expr accessors =
    fromExpression expr
        |> Pretty.a (Pretty.join Pretty.empty <| List.map fromAccessor accessors)


fromAccessor : Accessor Expression -> Pretty.Doc t
fromAccessor accessor =
    case accessor of
        Computed expr ->
            fromExpression expr
                |> Pretty.brackets

        Fixed field ->
            Pretty.char '.'
                |> Pretty.a (Pretty.string field)



-- EMITTING EXPRESSIONS: APPLICATION -------------------------------------------


fromApplication : Expression -> List Expression -> Pretty.Doc t
fromApplication expr args =
    fromExpression expr
        |> Pretty.a Pretty.space
        |> Pretty.a
            (args
                |> List.map (fromExpression >> Pretty.parens)
                |> Pretty.softlines
            )



-- EMITTING EXPRESSIONS: CONDITIONAL -------------------------------------------


fromConditional : Expression -> Expression -> Expression -> Pretty.Doc t
fromConditional condition true false =
    fromExpression condition
        |> Pretty.a Pretty.space
        |> Pretty.a (Pretty.char '?')
        |> Pretty.a Pretty.space
        |> Pretty.a (fromExpression true)
        |> Pretty.a Pretty.space
        |> Pretty.a (Pretty.char ':')
        |> Pretty.a Pretty.space
        |> Pretty.a (fromExpression false)



-- EMITTING EXPRESSIONS: IDENTIFIER --------------------------------------------


fromIdentifier : Identifier -> Pretty.Doc t
fromIdentifier identifier =
    case identifier of
        Local name ->
            Pretty.string name

        Scoped namespace name ->
            List.map Pretty.string namespace
                |> Pretty.join (Pretty.char '$')
                |> Pretty.a (Pretty.char '.')
                |> Pretty.a (Pretty.string name)

        Operator Pipe ->
            Pretty.string "$Function.pipe"

        Operator Compose ->
            Pretty.string "$Function.compose"

        Operator Add ->
            Pretty.string "$Math.add"

        Operator Sub ->
            Pretty.string "$Math.sub"

        Operator Mul ->
            Pretty.string "$Math.mul"

        Operator Div ->
            Pretty.string "$Math.div"

        Operator Pow ->
            Pretty.string "$Math.pow"

        Operator Mod ->
            Pretty.string "$Math.mod"

        Operator Eq ->
            Pretty.string "$Compare.eq"

        Operator NotEq ->
            Pretty.string "$Compare.noteq"

        Operator Lt ->
            Pretty.string "$Compare.lt"

        Operator Lte ->
            Pretty.string "$Compare.lte"

        Operator Gt ->
            Pretty.string "$Compare.gt"

        Operator Gte ->
            Pretty.string "$Compare.gte"

        Operator And ->
            Pretty.string "$Logic.and"

        Operator Or ->
            Pretty.string "$Logic.or"

        Operator Cons ->
            Pretty.string "$Array.cons"

        Operator Join ->
            Pretty.string "$Array.join"

        Field field ->
            Pretty.char '$'
                |> Pretty.parens
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "=>")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '$')
                |> Pretty.a (Pretty.char '.')
                |> Pretty.a (Pretty.string field)
                |> Pretty.parens



-- EMITTING EXPRESSIONS: INFIX -------------------------------------------------


fromInfix : Operator -> Expression -> Expression -> Pretty.Doc t
fromInfix operator lhs rhs =
    case operator of
        Pipe ->
            fromExpression (Application rhs [ lhs ])

        Compose ->
            Pretty.char '$'
                |> Pretty.parens
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "=>")
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (fromExpression
                        (Application rhs [ Application lhs [ Expression.local "$" ] ])
                    )
                |> Pretty.parens

        Add ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '+')
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Sub ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '-')
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Mul ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '*')
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Div ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '/')
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Pow ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "**")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Mod ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '%')
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Eq ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "==")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        NotEq ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "!=")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Lt ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '<')
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Lte ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "<=")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Gt ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '>')
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Gte ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string ">=")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        And ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "&&")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Or ->
            fromExpression lhs
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "||")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression rhs)

        Cons ->
            fromExpression lhs
                |> Pretty.a (Pretty.char ',')
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "...")
                |> Pretty.a (fromExpression rhs)
                |> Pretty.brackets

        Join ->
            Pretty.string "..."
                |> Pretty.a (fromExpression lhs)
                |> Pretty.a (Pretty.char ',')
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "...")
                |> Pretty.a (fromExpression rhs)
                |> Pretty.brackets



-- EMITTING EXPRESSIONS: LAMBDA ------------------------------------------------


fromLambda : List Pattern -> Expression -> Pretty.Doc t
fromLambda args body =
    List.map (fromPattern >> Pretty.parens) args
        |> Pretty.join (Pretty.space |> Pretty.a (Pretty.string "=>") |> Pretty.a Pretty.space)
        |> Pretty.a Pretty.space
        |> Pretty.a (fromExpression body)



-- EMITTING EXPRESSIONS: LITERAL -----------------------------------------------


fromLiteral : Literal Expression -> Pretty.Doc t
fromLiteral literal =
    case literal of
        Array elements ->
            List.map fromExpression elements
                |> Pretty.join (Pretty.char ',' |> Pretty.a Pretty.space)
                |> Pretty.brackets

        Boolean True ->
            Pretty.string "true"

        Boolean False ->
            Pretty.string "false"

        Number n ->
            String.fromFloat n
                |> Pretty.string

        Object entries ->
            Dict.toList entries
                |> List.map
                    (\( k, v ) ->
                        Pretty.string k
                            |> Pretty.a (Pretty.char ':')
                            |> Pretty.a Pretty.space
                            |> Pretty.a (fromExpression v)
                    )
                |> Pretty.join (Pretty.char ',' |> Pretty.a Pretty.space)
                |> Pretty.braces

        String s ->
            quotes s

        Undefined ->
            Pretty.string "undefined"


fromPrimitive : Literal Never -> Pretty.Doc t
fromPrimitive literal =
    case literal of
        -- Primitive literals can `Never` be an Array or an Object.
        Array _ ->
            Pretty.empty

        Boolean True ->
            Pretty.string "true"

        Boolean False ->
            Pretty.string "false"

        Number n ->
            String.fromFloat n
                |> Pretty.string

        -- Primitive literals can `Never` be an Array or an Object.
        Object _ ->
            Pretty.empty

        String s ->
            quotes s

        Undefined ->
            Pretty.string "undefined"



-- EMITTING EXPRESSIONS: MATCH -------------------------------------------------


fromMatch : Expression -> List ( Pattern, Maybe Expression, Expression ) -> Pretty.Doc t
fromMatch expr cases =
    Pretty.char '$'
        |> Pretty.parens
        |> Pretty.a Pretty.space
        |> Pretty.a (Pretty.string "=>")
        |> Pretty.a Pretty.space
        |> Pretty.a (Pretty.char '{')
        |> Pretty.a Pretty.line
        |> Pretty.a
            (List.map fromCase cases
                |> List.intersperse (Pretty.line |> Pretty.a Pretty.line)
                |> Pretty.join Pretty.empty
                |> Pretty.indent 4
            )
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.char '}')
        |> Pretty.parens
        |> Pretty.a (fromExpression expr |> Pretty.parens)


fromCase : ( Pattern, Maybe Expression, Expression ) -> Pretty.Doc t
fromCase ( pattern, guard, body ) =
    case pattern of
        ArrayDestructure patterns ->
            let
                matchPatterns =
                    matchPatternsFromArrayDestructure (Pretty.char '$') patterns

                checks =
                    List.map checkFromMatchPattern matchPatterns
                        |> List.filter ((/=) Pretty.empty)
                        |> (\checks_ ->
                                if List.isEmpty checks_ then
                                    Pretty.empty

                                else
                                    Pretty.join
                                        (Pretty.space
                                            |> Pretty.a (Pretty.string "&&")
                                            |> Pretty.a Pretty.space
                                        )
                                        checks_
                           )

                bindings =
                    List.map bindingFromMatchPattern matchPatterns
                        |> List.filter ((/=) Pretty.empty)
                        |> (\bindings_ ->
                                if List.isEmpty bindings_ then
                                    Pretty.empty

                                else
                                    Pretty.lines bindings_
                           )
            in
            Pretty.string "if"
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.parens checks)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (bindings
                                        |> Pretty.a Pretty.line
                                        |> Pretty.a (Pretty.string "return")
                                        |> Pretty.a Pretty.space
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            bindings
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "return")
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        Name name ->
            Pretty.string "if"
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (Pretty.char '$'
                        |> Pretty.a Pretty.space
                        |> Pretty.a (Pretty.string "==")
                        |> Pretty.a Pretty.space
                        |> Pretty.a (Pretty.string name)
                        |> Pretty.parens
                    )
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return"
                                        |> Pretty.a Pretty.space
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            Pretty.string "return"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        ObjectDestructure patterns ->
            let
                matchPatterns =
                    matchPatternsFromObjectDestructure (Pretty.char '$') patterns

                checks =
                    List.map checkFromMatchPattern matchPatterns
                        |> List.filter ((/=) Pretty.empty)
                        |> (\checks_ ->
                                if List.isEmpty checks_ then
                                    Pretty.empty

                                else
                                    Pretty.join
                                        (Pretty.space
                                            |> Pretty.a (Pretty.string "&&")
                                            |> Pretty.a Pretty.space
                                        )
                                        checks_
                           )

                bindings =
                    List.map bindingFromMatchPattern matchPatterns
                        |> List.filter ((/=) Pretty.empty)
                        |> (\bindings_ ->
                                if List.isEmpty bindings_ then
                                    Pretty.empty

                                else
                                    Pretty.lines bindings_
                           )

                _ =
                    Debug.log "" (Pretty.pretty 1000 bindings)
            in
            Pretty.string "if"
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.parens checks)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (bindings
                                        |> Pretty.a Pretty.line
                                        |> Pretty.a (Pretty.string "return")
                                        |> Pretty.a Pretty.space
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            bindings
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.string "return")
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        Value primitive ->
            Pretty.string "if"
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (Pretty.char '$'
                        |> Pretty.a Pretty.space
                        |> Pretty.a (Pretty.string "==")
                        |> Pretty.a Pretty.space
                        |> Pretty.a (fromPrimitive primitive)
                        |> Pretty.parens
                    )
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return"
                                        |> Pretty.a Pretty.space
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            Pretty.string "return"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        Wildcard _ ->
            case guard of
                Just expr ->
                    Pretty.string "if"
                        |> Pretty.a Pretty.space
                        |> Pretty.a (fromExpression expr |> Pretty.parens)
                        |> Pretty.a Pretty.space
                        |> Pretty.a (Pretty.char '{')
                        |> Pretty.a Pretty.line
                        |> Pretty.a
                            (Pretty.string "return"
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                            )
                        |> Pretty.a Pretty.line
                        |> Pretty.a (Pretty.char '}')

                Nothing ->
                    Pretty.string "return"
                        |> Pretty.a Pretty.space
                        |> Pretty.a (fromExpression body)


type MatchPattern t
    = Existential { name : Pretty.Doc t, path : Pretty.Doc t }
    | Equality { value : Pretty.Doc t, path : Pretty.Doc t }
    | Binding { name : Pretty.Doc t, path : Pretty.Doc t }
    | IsArray { path : Pretty.Doc t, length : Int }
    | IsObject (Pretty.Doc t)


checkFromMatchPattern : MatchPattern t -> Pretty.Doc t
checkFromMatchPattern pattern =
    case pattern of
        Existential { name, path } ->
            name
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "in")
                |> Pretty.a Pretty.space
                |> Pretty.a path

        Equality { value, path } ->
            path
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "==")
                |> Pretty.a Pretty.space
                |> Pretty.a value

        Binding _ ->
            Pretty.empty

        IsArray { path, length } ->
            Pretty.string "Array.isArray"
                |> Pretty.a (Pretty.parens path)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "&&")
                |> Pretty.a Pretty.space
                |> Pretty.a path
                |> Pretty.a (Pretty.string ".length")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string ">=")
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (String.fromInt length
                        |> Pretty.string
                    )

        IsObject path ->
            Pretty.string "typeof"
                |> Pretty.a Pretty.space
                |> Pretty.a path
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "==")
                |> Pretty.a Pretty.space
                |> Pretty.a (quotes "object")


bindingFromMatchPattern : MatchPattern t -> Pretty.Doc t
bindingFromMatchPattern pattern =
    case pattern of
        Existential _ ->
            Pretty.empty

        Equality _ ->
            Pretty.empty

        Binding { name, path } ->
            Pretty.string "var"
                |> Pretty.a Pretty.space
                |> Pretty.a name
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "=")
                |> Pretty.a Pretty.space
                |> Pretty.a path

        IsArray _ ->
            Pretty.empty

        IsObject _ ->
            Pretty.empty


matchPatternsFromArrayDestructure : Pretty.Doc t -> List Pattern -> List (MatchPattern t)
matchPatternsFromArrayDestructure path patterns =
    patterns
        |> List.indexedMap
            (\i pattern ->
                let
                    idx =
                        String.fromInt i
                            |> Pretty.string
                            |> Pretty.brackets
                in
                case pattern of
                    ArrayDestructure ps ->
                        matchPatternsFromArrayDestructure
                            (Pretty.a idx path)
                            ps

                    Name name ->
                        [ Binding
                            { name = Pretty.string name
                            , path = Pretty.a idx path
                            }
                        ]

                    ObjectDestructure ps ->
                        matchPatternsFromObjectDestructure
                            (Pretty.a idx path)
                            ps

                    Value primitive ->
                        [ Equality
                            { value = fromPrimitive primitive
                            , path = Pretty.a idx path
                            }
                        ]

                    Wildcard _ ->
                        []
            )
        |> List.concat
        |> (::) (IsArray { path = path, length = List.length patterns })


matchPatternsFromObjectDestructure : Pretty.Doc t -> List ( String, Maybe Pattern ) -> List (MatchPattern t)
matchPatternsFromObjectDestructure path patterns =
    patterns
        |> List.concatMap
            (\( key, pattern ) ->
                case pattern of
                    Just (ArrayDestructure ps) ->
                        matchPatternsFromArrayDestructure
                            (path
                                |> Pretty.a (Pretty.char '.')
                                |> Pretty.a (Pretty.string key)
                            )
                            ps

                    Just (Name name) ->
                        [ Existential
                            { name = quotes key
                            , path = path
                            }
                        , Binding
                            { name = Pretty.string name
                            , path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            }
                        ]

                    Just (ObjectDestructure ps) ->
                        matchPatternsFromObjectDestructure
                            (path
                                |> Pretty.a (Pretty.char '.')
                                |> Pretty.a (Pretty.string key)
                            )
                            ps

                    Just (Value primitive) ->
                        [ Existential
                            { name = quotes key
                            , path = path
                            }
                        , Equality
                            { value = fromPrimitive primitive
                            , path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            }
                        ]

                    Just (Wildcard _) ->
                        [ Existential
                            { name = quotes key
                            , path = path
                            }
                        ]

                    Nothing ->
                        [ Existential
                            { name = quotes key
                            , path = path
                            }
                        , Binding
                            { name = Pretty.string key
                            , path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            }
                        ]
            )
        |> (::) (IsObject path)



-- EMITTING EXPRESSIONS: SUBEXPRESSION -----------------------------------------


fromSubexpression : Expression -> Pretty.Doc t
fromSubexpression expr =
    fromExpression expr
        |> Pretty.parens



-- EMITTING PATTERNS -----------------------------------------------------------


fromPattern : Pattern -> Pretty.Doc t
fromPattern pattern =
    let
        -- This replaces literal patterns inside array and object destructures
        -- with placeholder wildcard matches.
        --
        -- TODO: We need to come up with a proper way to handle literal patterns
        -- in things like function arguments. If I write:
        --
        --     fun [ x, 1, z ] => ...
        --
        -- I'd only expect this function to run when the second element in the
        -- array is `1`. With the behaviour described above, this function always
        -- runs regardless of the value of the second element.
        replaceLiteral p =
            case p of
                ArrayDestructure ps ->
                    List.map replaceLiteral ps
                        |> ArrayDestructure

                Name _ ->
                    p

                ObjectDestructure ps ->
                    List.map (Tuple.mapSecond (Maybe.map replaceLiteral)) ps
                        |> ObjectDestructure

                Value _ ->
                    Wildcard Nothing

                Wildcard _ ->
                    p
    in
    case pattern of
        ArrayDestructure patterns ->
            List.map replaceLiteral patterns
                |> List.map fromPattern
                |> Pretty.join (Pretty.char ',' |> Pretty.a Pretty.space)
                |> Pretty.brackets

        Name name ->
            Pretty.string name

        ObjectDestructure patterns ->
            List.map (Tuple.mapSecond (Maybe.map replaceLiteral)) patterns
                |> List.map
                    (\( k, v ) ->
                        case v of
                            Just p ->
                                Pretty.string k
                                    |> Pretty.a (Pretty.char ':')
                                    |> Pretty.a Pretty.space
                                    |> Pretty.a (fromPattern p)

                            Nothing ->
                                Pretty.string k
                    )
                |> Pretty.join (Pretty.char ',' |> Pretty.a Pretty.space)
                |> Pretty.braces

        Value primitive ->
            fromPrimitive primitive

        Wildcard (Just name) ->
            Pretty.char '_'
                |> Pretty.a (Pretty.string name)

        Wildcard Nothing ->
            Pretty.char '_'



-- UTILS -----------------------------------------------------------------------


quotes : String -> Pretty.Doc t
quotes s =
    Pretty.string s
        |> Pretty.surround (Pretty.char '\'') (Pretty.char '\'')
