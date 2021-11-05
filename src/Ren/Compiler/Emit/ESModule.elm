module Ren.Compiler.Emit.ESModule exposing
    ( emitDeclaration
    , emitExpression
    , emitModule
    )

-- IMPORTS ---------------------------------------------------------------------

import Dict
import Pretty
import Pretty.Extra
import Ren.Language exposing (..)



-- EMITTING MODULES ------------------------------------------------------------


{-| -}
emitModule : Module -> String
emitModule =
    fromModule >> Pretty.pretty 80


fromModule : Module -> Pretty.Doc t
fromModule { imports, declarations } =
    if List.isEmpty imports then
        declarations
            |> List.map (fromDeclaration >> Pretty.a Pretty.line)
            |> Pretty.lines

    else
        Pretty.lines (List.map fromImport imports)
            |> Pretty.a Pretty.line
            |> Pretty.a Pretty.line
            |> Pretty.a
                (declarations
                    |> List.map (fromDeclaration >> Pretty.a Pretty.line)
                    |> Pretty.lines
                )


fromImport : Import -> Pretty.Doc t
fromImport ({ path, name, bindings } as import_) =
    case ( List.isEmpty name, List.isEmpty bindings ) of
        ( True, True ) ->
            Pretty.string "import "
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.Extra.singleQuotes path)

        ( True, False ) ->
            Pretty.string "import "
                |> Pretty.a
                    (List.map (String.replace "#" "$" >> Pretty.string) bindings
                        |> Pretty.join (Pretty.string ", ")
                        |> Pretty.braces
                    )
                |> Pretty.a (Pretty.string " from ")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.Extra.singleQuotes path)

        ( False, True ) ->
            Pretty.string "import * as "
                |> Pretty.a (Pretty.string <| String.join "$" name)
                |> Pretty.a (Pretty.string " from ")
                |> Pretty.a (Pretty.Extra.singleQuotes path)

        ( False, False ) ->
            Pretty.lines
                [ fromImport { import_ | bindings = [] }
                , fromImport { import_ | name = [] }
                ]



-- EMITTING DECLARATIONS -------------------------------------------------------


{-| -}
emitDeclaration : Declaration -> String
emitDeclaration =
    Tuple.pair Private >> fromDeclaration >> Pretty.pretty 80


fromDeclaration : ( Visibility, Declaration ) -> Pretty.Doc t
fromDeclaration ( visibility, declaration ) =
    case declaration of
        Function name args body ->
            fromVisibility visibility
                |> Pretty.a (fromFunction (Name name) args body)

        Variable name body ->
            fromVisibility visibility
                |> Pretty.a (fromVariable name body)

        Enum name variants ->
            fromEnum visibility name variants



-- EMIITTING DECLARATIONS: FUNCTIONS -------------------------------------------


fromFunction : Pattern -> List Pattern -> Expression -> Pretty.Doc t
fromFunction name args body =
    case args of
        -- Ren doesn't allow nullary function definitions, so this case should
        -- never be hit.
        [] ->
            Pretty.empty

        arg :: [] ->
            Pretty.string "function "
                |> Pretty.a (fromPattern name)
                |> Pretty.a Pretty.space
                |> Pretty.a (fromPattern arg |> Pretty.parens)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (Pretty.string "return "
                        |> Pretty.a (fromExpression body)
                        |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        arg :: rest ->
            Pretty.string "function "
                |> Pretty.a (fromPattern name)
                |> Pretty.a Pretty.space
                |> Pretty.a (fromPattern arg |> Pretty.parens)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (Pretty.string "return "
                        |> Pretty.a
                            (List.map (fromPattern >> Pretty.parens) rest
                                |> Pretty.join (Pretty.string " => ")
                                |> Pretty.a (Pretty.string " => ")
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return "
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


fromVariable : Pattern -> Expression -> Pretty.Doc t
fromVariable name body =
    Pretty.string "var "
        |> Pretty.a (fromPattern name)
        |> Pretty.a (Pretty.string " = ")
        |> Pretty.a (fromExpression body)



-- EMITTING DECLARATIONS: ENUMS ------------------------------------------------


fromEnum : Visibility -> String -> List Variant -> Pretty.Doc t
fromEnum visibility _ variants =
    variants
        |> List.map (fromVariant visibility)
        |> Pretty.join Pretty.Extra.doubeLine


fromVariant : Visibility -> Variant -> Pretty.Doc t
fromVariant visibility (Variant tag slots) =
    let
        sanitisedTag =
            String.replace "#" "$" tag

        -- OK this boldly assumes that no single variant will have more than 26
        -- slots. This seems like a reasonable assumption, if something breaks
        -- because of it, they deserve it.
        slotArgs =
            List.range 0 (slots - 1)
                |> List.map (\code -> Char.fromCode (97 + code) |> String.fromChar)
    in
    fromVisibility visibility
        |> Pretty.a
            (if slots <= 0 then
                fromVariable
                    (Name sanitisedTag)
                    (Literal
                        (Array [ Literal (String tag) ])
                    )

             else
                fromFunction
                    (Name sanitisedTag)
                    (List.map Name slotArgs)
                    (Literal
                        (Array
                            (Literal (String tag) :: List.map (Identifier << Local) slotArgs)
                        )
                    )
            )



-- EMITTING DECLARATIONS: VISIBILITY -------------------------------------------


fromVisibility : Visibility -> Pretty.Doc t
fromVisibility visibility =
    case visibility of
        Public ->
            Pretty.string "export "

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

        Block bindings expr ->
            fromBlock bindings expr

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


{-| Should contain all expressions which when output either starts or ends with an expression
-}
fromExpressionToSingleTerm : Expression -> Pretty.Doc t
fromExpressionToSingleTerm expression =
    case expression of
        Application expr args ->
            fromApplication expr args
                |> Pretty.parens

        Conditional condition true false ->
            fromConditional condition true false
                |> Pretty.parens

        -- Compiles to Application: always wrap
        Infix Pipe lhs rhs ->
            fromInfix Pipe lhs rhs
                |> Pretty.parens

        -- Compiles to IIFE: never wrap
        Infix Compose lhs rhs ->
            fromInfix Pipe lhs rhs

        -- Compiles to []: never wrap
        Infix Cons lhs rhs ->
            fromInfix Pipe lhs rhs

        -- Compiles to []: never wrap
        Infix Join lhs rhs ->
            fromInfix Pipe lhs rhs

        Infix operator lhs rhs ->
            fromInfix operator lhs rhs

        Lambda args body ->
            fromLambda args body
                |> Pretty.parens

        _ ->
            fromExpression expression



-- EMITTING EXPRESSIONS: ACCESS ------------------------------------------------


fromAccess : Expression -> List Accessor -> Pretty.Doc t
fromAccess expr accessors =
    fromExpressionToSingleTerm expr
        |> Pretty.a (Pretty.join Pretty.empty <| List.map fromAccessor accessors)


fromAccessor : Accessor -> Pretty.Doc t
fromAccessor accessor =
    case accessor of
        Computed expr ->
            fromExpression expr
                |> Pretty.brackets

        Fixed field ->
            Pretty.string ("." ++ field)



-- EMITTING EXPRESSIONS: APPLICATION -------------------------------------------


fromApplication : Expression -> List Expression -> Pretty.Doc t
fromApplication expr args =
    fromExpressionToSingleTerm expr
        |> Pretty.a Pretty.space
        |> Pretty.a
            (args
                |> List.map fromArgument
                |> Pretty.lines
            )
        |> Pretty.group
        |> Pretty.nest 4


fromArgument : Expression -> Pretty.Doc t
fromArgument arg =
    case arg of
        Literal Undefined ->
            Pretty.string "()"

        _ ->
            fromExpression arg
                |> Pretty.parens



-- EMITTING EXPRESSIONS: BLOCK -------------------------------------------------


fromBlock : List Declaration -> Expression -> Pretty.Doc t
fromBlock bindings expr =
    Pretty.string "() => {"
        |> Pretty.a Pretty.line
        |> Pretty.a
            (List.map (Tuple.pair Private >> fromDeclaration) bindings
                |> List.intersperse Pretty.Extra.doubeLine
                |> Pretty.join Pretty.empty
                |> Pretty.a Pretty.Extra.doubeLine
                |> Pretty.a (Pretty.string "return ")
                |> Pretty.a (fromExpression expr)
                |> Pretty.indent 4
            )
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.char '}')
        |> Pretty.parens
        |> Pretty.a (Pretty.string "()")



-- EMITTING EXPRESSIONS: CONDITIONAL -------------------------------------------


fromConditional : Expression -> Expression -> Expression -> Pretty.Doc t
fromConditional condition true false =
    fromExpression condition
        |> Pretty.a Pretty.line
        |> Pretty.a
            (Pretty.string "? "
                |> Pretty.a (fromExpression true)
                |> Pretty.indent 4
            )
        |> Pretty.a Pretty.line
        |> Pretty.a
            (Pretty.string ": "
                |> Pretty.a (fromExpression false)
                |> Pretty.indent 4
            )



-- EMITTING EXPRESSIONS: IDENTIFIER --------------------------------------------


fromIdentifier : Identifier -> Pretty.Doc t
fromIdentifier identifier =
    case identifier of
        Local name ->
            Pretty.string name

        Constructor name ->
            String.replace "#" "$" name
                |> Pretty.string

        Scoped namespace name ->
            List.map Pretty.string namespace
                |> Pretty.join (Pretty.char '$')
                |> Pretty.a (Pretty.char '.')
                |> Pretty.a (fromIdentifier name)

        Operator Pipe ->
            Pretty.string "Function.pipe"

        Operator Compose ->
            Pretty.string "Function.compose"

        Operator Add ->
            Pretty.string "Math.add"

        Operator Sub ->
            Pretty.string "Math.sub"

        Operator Mul ->
            Pretty.string "Math.mul"

        Operator Div ->
            Pretty.string "Math.div"

        Operator Pow ->
            Pretty.string "Math.pow"

        Operator Mod ->
            Pretty.string "Math.mod"

        Operator Eq ->
            Pretty.string "Compare.eq"

        Operator NotEq ->
            Pretty.string "Compare.noteq"

        Operator Lt ->
            Pretty.string "Compare.lt"

        Operator Lte ->
            Pretty.string "Compare.lte"

        Operator Gt ->
            Pretty.string "Compare.gt"

        Operator Gte ->
            Pretty.string "Compare.gte"

        Operator And ->
            Pretty.string "Logic.and"

        Operator Or ->
            Pretty.string "Logic.or"

        Operator Cons ->
            Pretty.string "Array.cons"

        Operator Join ->
            Pretty.string "Array.join"

        Field field ->
            Pretty.char '$'
                |> Pretty.parens
                |> Pretty.a (Pretty.string " => $.")
                |> Pretty.a (Pretty.string field)
                |> Pretty.parens



-- EMITTING EXPRESSIONS: INFIX -------------------------------------------------


fromInfix : Operator -> Expression -> Expression -> Pretty.Doc t
fromInfix operator lhs rhs =
    let
        fromInfixOp =
            fromInfixOperand operator
    in
    case operator of
        Pipe ->
            fromExpression (Application rhs [ lhs ])

        Compose ->
            Pretty.char '$'
                |> Pretty.parens
                |> Pretty.a (Pretty.string " => ")
                |> Pretty.a
                    (fromExpression
                        (Application rhs [ Application lhs [ Identifier (Local "$") ] ])
                    )
                |> Pretty.parens

        Add ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " + ")
                |> Pretty.a (fromInfixOp rhs)

        Sub ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " - ")
                |> Pretty.a (fromInfixOp rhs)

        Mul ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " * ")
                |> Pretty.a (fromInfixOp rhs)

        Div ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " / ")
                |> Pretty.a (fromInfixOp rhs)

        Pow ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " ** ")
                |> Pretty.a (fromInfixOp rhs)

        Mod ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " % ")
                |> Pretty.a (fromInfixOp rhs)

        Eq ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " == ")
                |> Pretty.a (fromInfixOp rhs)

        NotEq ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " != ")
                |> Pretty.a (fromInfixOp rhs)

        Lt ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " < ")
                |> Pretty.a (fromInfixOp rhs)

        Lte ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " <= ")
                |> Pretty.a (fromInfixOp rhs)

        Gt ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " > ")
                |> Pretty.a (fromInfixOp rhs)

        Gte ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " >= ")
                |> Pretty.a (fromInfixOp rhs)

        And ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " && ")
                |> Pretty.a (fromInfixOp rhs)

        Or ->
            fromInfixOp lhs
                |> Pretty.a (Pretty.string " || ")
                |> Pretty.a (fromInfixOp rhs)

        Cons ->
            fromExpressionToSingleTerm lhs
                |> Pretty.a (Pretty.string ", ...")
                |> Pretty.a (fromExpressionToSingleTerm rhs)
                |> Pretty.brackets

        Join ->
            Pretty.string "..."
                |> Pretty.a (fromExpressionToSingleTerm lhs)
                |> Pretty.a (Pretty.string ", ...")
                |> Pretty.a (fromExpressionToSingleTerm rhs)
                |> Pretty.brackets


{-| Uses precedence numbers from
<https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence>
-}
esPrecedenceFromInfixOperator : Operator -> Maybe Int
esPrecedenceFromInfixOperator operator =
    case operator of
        Add ->
            Just 14

        Sub ->
            Just 14

        Mul ->
            Just 15

        Div ->
            Just 15

        Pow ->
            Just 16

        Mod ->
            Just 15

        Eq ->
            Just 11

        NotEq ->
            Just 11

        Lt ->
            Just 12

        Lte ->
            Just 12

        Gt ->
            Just 12

        Gte ->
            Just 12

        And ->
            Just 7

        Or ->
            Just 6

        _ ->
            Nothing


fromInfixOperand : Operator -> Expression -> Pretty.Doc t
fromInfixOperand parentOperator operand =
    let
        parensRequired =
            \operator ->
                case ( esPrecedenceFromInfixOperator parentOperator, esPrecedenceFromInfixOperator operator ) of
                    ( Just pParent, Just pChild ) ->
                        pParent > pChild

                    _ ->
                        False
    in
    case operand of
        Infix operator lhs rhs ->
            if parensRequired operator then
                fromInfix operator lhs rhs
                    |> Pretty.parens

            else
                fromInfix operator lhs rhs

        _ ->
            fromExpressionToSingleTerm operand



-- EMITTING EXPRESSIONS: LAMBDA ------------------------------------------------


fromLambda : List Pattern -> Expression -> Pretty.Doc t
fromLambda args body =
    List.map (fromPattern >> Pretty.parens) args
        |> Pretty.join (Pretty.string " => ")
        |> Pretty.a (Pretty.string " => ")
        |> Pretty.a (fromExpression body)



-- EMITTING EXPRESSIONS: LITERAL -----------------------------------------------


fromLiteral : Literal -> Pretty.Doc t
fromLiteral literal =
    case literal of
        Array elements ->
            List.map fromExpression elements
                |> Pretty.join (Pretty.string ", ")
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
                            |> Pretty.a (Pretty.string ": ")
                            |> Pretty.a (fromExpression v)
                    )
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.braces

        String s ->
            Pretty.Extra.singleQuotes s

        Template parts ->
            List.map
                (\part ->
                    case part of
                        Text text ->
                            Pretty.string text

                        Expr expr ->
                            fromExpression expr
                                |> Pretty.surround (Pretty.string "${") (Pretty.char '}')
                )
                parts
                |> Pretty.join Pretty.empty
                |> Pretty.surround (Pretty.char '`') (Pretty.char '`')

        Undefined ->
            Pretty.string "undefined"



-- EMITTING EXPRESSIONS: MATCH -------------------------------------------------


fromMatch : Expression -> List ( Pattern, Maybe Expression, Expression ) -> Pretty.Doc t
fromMatch expr cases =
    Pretty.char '$'
        |> Pretty.parens
        |> Pretty.a (Pretty.string " => ")
        |> Pretty.a (Pretty.char '{')
        |> Pretty.a Pretty.line
        |> Pretty.a
            (List.map fromCase cases
                |> List.intersperse Pretty.Extra.doubeLine
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
                                    Pretty.join (Pretty.string " && ") checks_
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
            Pretty.string "if "
                |> Pretty.a (Pretty.parens checks)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.indent 4 bindings)
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if "
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return "
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            Pretty.string "return "
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        Name name ->
            Pretty.string "if "
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (Pretty.char '$'
                        |> Pretty.a (Pretty.string " == ")
                        |> Pretty.a (Pretty.string name)
                        |> Pretty.parens
                    )
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (Pretty.string "var "
                        |> Pretty.a (Pretty.string name)
                        |> Pretty.a (Pretty.string " = $")
                        |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if "
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return "
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            Pretty.string "return "
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
                                    Pretty.join (Pretty.string " && ") checks_
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
            Pretty.string "if "
                |> Pretty.a (Pretty.parens checks)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.indent 4 bindings)
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if "
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return "
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            Pretty.string "return "
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
                        |> Pretty.a (Pretty.string " == ")
                        |> Pretty.a (fromLiteral primitive)
                        |> Pretty.parens
                    )
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if "
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return "
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            Pretty.string "return "
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        VariantDestructure tag patterns ->
            fromCase
                ( ArrayDestructure (Value (String tag) :: patterns)
                , guard
                , body
                )

        Typeof typeof name ->
            let
                typename =
                    case typeof of
                        BooleanP ->
                            "boolean"

                        NumberP ->
                            "number"

                        StringP ->
                            "string"

                        FunctionP ->
                            "function"
            in
            Pretty.string "if "
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (Pretty.string "typeof $ == "
                        |> Pretty.a (Pretty.string typename)
                        |> Pretty.parens
                    )
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (Pretty.string "var "
                        |> Pretty.a (Pretty.string name)
                        |> Pretty.a (Pretty.string " = $")
                        |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case guard of
                        Just expr ->
                            Pretty.string "if "
                                |> Pretty.a (fromExpression expr |> Pretty.parens)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return "
                                        |> Pretty.a (fromExpression body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.indent 4

                        Nothing ->
                            Pretty.string "return "
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')

        Wildcard _ ->
            case guard of
                Just expr ->
                    Pretty.string "if "
                        |> Pretty.a (fromExpression expr |> Pretty.parens)
                        |> Pretty.a Pretty.space
                        |> Pretty.a (Pretty.char '{')
                        |> Pretty.a Pretty.line
                        |> Pretty.a
                            (Pretty.string "return "
                                |> Pretty.a (fromExpression body)
                                |> Pretty.indent 4
                            )
                        |> Pretty.a Pretty.line
                        |> Pretty.a (Pretty.char '}')

                Nothing ->
                    Pretty.string "return "
                        |> Pretty.a (fromExpression body)


type MatchPattern t
    = Existential { name : Pretty.Doc t, path : Pretty.Doc t }
    | Equality { value : Pretty.Doc t, path : Pretty.Doc t }
    | Binding { name : Pretty.Doc t, path : Pretty.Doc t }
    | IsArray { path : Pretty.Doc t, length : Int }
    | IsObject (Pretty.Doc t)
    | IsType { path : Pretty.Doc t, typeof : Pretty.Doc t }


checkFromMatchPattern : MatchPattern t -> Pretty.Doc t
checkFromMatchPattern pattern =
    case pattern of
        Existential { name, path } ->
            name
                |> Pretty.a (Pretty.string " in ")
                |> Pretty.a path

        Equality { value, path } ->
            path
                |> Pretty.a (Pretty.string " == ")
                |> Pretty.a value

        Binding _ ->
            Pretty.empty

        IsArray { path, length } ->
            Pretty.string "Array.isArray"
                |> Pretty.a (Pretty.parens path)
                |> Pretty.a (Pretty.string " && ")
                |> Pretty.a path
                |> Pretty.a (Pretty.string ".length")
                |> Pretty.a (Pretty.string " >= ")
                |> Pretty.a
                    (String.fromInt length
                        |> Pretty.string
                    )

        IsObject path ->
            Pretty.string "typeof "
                |> Pretty.a path
                |> Pretty.a (Pretty.string " == ")
                |> Pretty.a (Pretty.Extra.singleQuotes "object")

        IsType { path, typeof } ->
            Pretty.string "typeof "
                |> Pretty.a path
                |> Pretty.a (Pretty.string " == ")
                |> Pretty.a typeof


bindingFromMatchPattern : MatchPattern t -> Pretty.Doc t
bindingFromMatchPattern pattern =
    case pattern of
        Existential _ ->
            Pretty.empty

        Equality _ ->
            Pretty.empty

        Binding { name, path } ->
            Pretty.string "var "
                |> Pretty.a name
                |> Pretty.a (Pretty.string " = ")
                |> Pretty.a path

        IsArray _ ->
            Pretty.empty

        IsObject _ ->
            Pretty.empty

        IsType _ ->
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
                            { value = fromLiteral primitive
                            , path = Pretty.a idx path
                            }
                        ]

                    VariantDestructure tag ps ->
                        matchPatternsFromArrayDestructure
                            (Pretty.a idx path)
                            (Value (String tag) :: ps)

                    Typeof BooleanP name ->
                        [ IsType
                            { path = Pretty.a idx path
                            , typeof = Pretty.Extra.singleQuotes "boolean"
                            }
                        , Binding
                            { name = Pretty.string name
                            , path = Pretty.a idx path
                            }
                        ]

                    Typeof NumberP name ->
                        [ IsType
                            { path = Pretty.a idx path
                            , typeof = Pretty.Extra.singleQuotes "number"
                            }
                        , Binding
                            { name = Pretty.string name
                            , path = Pretty.a idx path
                            }
                        ]

                    Typeof StringP name ->
                        [ IsType
                            { path = Pretty.a idx path
                            , typeof = Pretty.Extra.singleQuotes "string"
                            }
                        , Binding
                            { name = Pretty.string name
                            , path = Pretty.a idx path
                            }
                        ]

                    Typeof FunctionP name ->
                        [ IsType
                            { path = Pretty.a idx path
                            , typeof = Pretty.Extra.singleQuotes "function"
                            }
                        , Binding
                            { name = Pretty.string name
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
                            { name = Pretty.Extra.singleQuotes key
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
                            { name = Pretty.Extra.singleQuotes key
                            , path = path
                            }
                        , Equality
                            { value = fromLiteral primitive
                            , path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            }
                        ]

                    Just (Wildcard _) ->
                        [ Existential
                            { name = Pretty.Extra.singleQuotes key
                            , path = path
                            }
                        ]

                    Just (VariantDestructure tag ps) ->
                        matchPatternsFromArrayDestructure
                            (path
                                |> Pretty.a (Pretty.char '.')
                                |> Pretty.a (Pretty.string key)
                            )
                            (Value (String tag) :: ps)

                    Just (Typeof BooleanP name) ->
                        [ Existential
                            { name = Pretty.Extra.singleQuotes key
                            , path = path
                            }
                        , IsType
                            { path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            , typeof = Pretty.Extra.singleQuotes "boolean"
                            }
                        , Binding
                            { name = Pretty.string name
                            , path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            }
                        ]

                    Just (Typeof NumberP name) ->
                        [ Existential
                            { name = Pretty.Extra.singleQuotes key
                            , path = path
                            }
                        , IsType
                            { path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            , typeof = Pretty.Extra.singleQuotes "number"
                            }
                        , Binding
                            { name = Pretty.string name
                            , path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            }
                        ]

                    Just (Typeof StringP name) ->
                        [ Existential
                            { name = Pretty.Extra.singleQuotes key
                            , path = path
                            }
                        , IsType
                            { path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            , typeof = Pretty.Extra.singleQuotes "string"
                            }
                        , Binding
                            { name = Pretty.string name
                            , path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            }
                        ]

                    Just (Typeof FunctionP name) ->
                        [ Existential
                            { name = Pretty.Extra.singleQuotes key
                            , path = path
                            }
                        , IsType
                            { path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            , typeof = Pretty.Extra.singleQuotes "function"
                            }
                        , Binding
                            { name = Pretty.string name
                            , path =
                                path
                                    |> Pretty.a (Pretty.char '.')
                                    |> Pretty.a (Pretty.string key)
                            }
                        ]

                    Nothing ->
                        [ Existential
                            { name = Pretty.Extra.singleQuotes key
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

                VariantDestructure tag ps ->
                    List.map replaceLiteral ps
                        |> VariantDestructure tag

                Typeof _ name ->
                    p

                Wildcard _ ->
                    p
    in
    case pattern of
        ArrayDestructure patterns ->
            List.map replaceLiteral patterns
                |> List.map fromPattern
                |> Pretty.join (Pretty.string ", ")
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
                                    |> Pretty.a (Pretty.string ": ")
                                    |> Pretty.a (fromPattern p)

                            Nothing ->
                                Pretty.string k
                    )
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.braces

        Value _ ->
            fromPattern (Wildcard Nothing)

        VariantDestructure tag patterns ->
            List.map replaceLiteral (Value (String tag) :: patterns)
                |> List.map fromPattern
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.brackets

        -- TODO: Like above, we need to come up with a proper way to handle these
        -- sorts of patterns in function args.
        Typeof _ name ->
            Pretty.string name

        Wildcard (Just name) ->
            Pretty.string ("_" ++ name)

        Wildcard Nothing ->
            Pretty.char '_'
