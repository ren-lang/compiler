module Ren.Compiler.Optimise.Expression exposing (optimise)

-- IMPORTS ---------------------------------------------------------------------

import Dict
import List.Extra
import Ren.Data.Expression as Expression exposing (Expression(..))
import Ren.Data.Expression.Accessor exposing (Accessor(..))
import Ren.Data.Expression.Identifier exposing (Identifier(..))
import Ren.Data.Expression.Literal as Literal exposing (Literal(..))
import Ren.Data.Expression.Operator exposing (Operator(..))
import Transform



-- RUNNING OPTIMISATIONS -------------------------------------------------------


{-| -}
optimise : Expression -> Expression
optimise =
    Transform.transformAll recursiveTransformation
        (Transform.orList
            [ constantFold
            , stripParentheses
            ]
        )


{-| -}
recursiveTransformation : (Expression -> Expression) -> Expression -> Expression
recursiveTransformation transform expression =
    case expression of
        Access expr accessors ->
            Access
                (transform expr)
                (List.map
                    (\accessor ->
                        case accessor of
                            Computed e ->
                                Computed (transform e)

                            Fixed s ->
                                Fixed s
                    )
                    accessors
                )

        Application func args ->
            Application
                (transform func)
                (List.map transform args)

        Conditional predicate true false ->
            Conditional
                (transform predicate)
                (transform true)
                (transform false)

        Identifier id ->
            Identifier id

        Infix op lhs rhs ->
            Infix op
                (transform lhs)
                (transform rhs)

        Lambda args body ->
            Lambda args
                (transform body)

        Literal (Array elements) ->
            Literal
                (Array <| List.map transform elements)

        Literal (Object fields) ->
            Literal
                (Object <| Dict.map (always transform) fields)

        Literal literal ->
            Literal literal

        Match expr patterns ->
            Match
                (transform expr)
                (List.map
                    (\( pattern, guard, body ) ->
                        ( pattern, Maybe.map transform guard, transform body )
                    )
                    patterns
                )

        SubExpression expr ->
            SubExpression (transform expr)



-- OPTIMISATIONS: CONSTANT FOLDING ---------------------------------------------


{-| -}
constantFold : Expression -> Maybe Expression
constantFold expression =
    case expression of
        -- ACCESS --------------------------------------------------------------
        -- Attempt to access an array literal by coercing the computed literal
        -- to an integer and then indexing the array with that string.
        Access (Literal (Array elements)) ((Computed (Literal n)) :: []) ->
            Literal.coerceToInteger n
                |> Maybe.andThen (\i -> List.Extra.at i elements)

        -- Attempt to access an array literal by coercing the computed literal
        -- to an integer and then indexing the array with that string, then turn
        -- that back into a `Access` expression using the remaining accessors.
        Access (Literal (Array elements)) ((Computed (Literal n)) :: accessors) ->
            Literal.coerceToInteger n
                |> Maybe.andThen (\i -> List.Extra.at i elements)
                |> Maybe.map (\arr -> Access arr accessors)

        -- Attempt to access an object literal with a fixed string key.
        Access (Literal (Object fields)) ((Fixed key) :: []) ->
            Dict.get key fields

        -- Attempt to access an object literal with a fixed string key, then turn
        -- that back into a `Access` expression using the remaining accessors.
        Access (Literal (Object fields)) ((Fixed key) :: accessors) ->
            Dict.get key fields
                |> Maybe.map (\obj -> Access obj accessors)

        -- Attempt to access an object literal by coercing the computed literal
        -- to a string and then indexing the object with that string, then turn
        -- that back into a `Access` expression using the remaining accessors.
        Access (Literal (Object fields)) ((Computed (Literal s)) :: []) ->
            Maybe.map2 Dict.get (Literal.coerceToString s) (Just fields)
                |> Maybe.andThen identity

        -- Attempt to access an object literal by coercing the computed literal
        -- to a string and then indexing the object with that string.
        Access (Literal (Object fields)) ((Computed (Literal s)) :: accessors) ->
            Maybe.map2 Dict.get (Literal.coerceToString s) (Just fields)
                |> Maybe.andThen identity
                |> Maybe.map (\obj -> Access obj accessors)

        -- APPLICATION ---------------------------------------------------------
        -- When using the function form of an object field accessor `.field obj`
        -- we can simplify that to just plain dot notation as long as the argument
        -- is an identifier (eg not a complicated expression)
        Application (Identifier (Field key)) [ Identifier id ] ->
            Access (Identifier id) [ Fixed key ]
                |> Just

        -- When using the function form of an object field accessor `.field obj`
        -- we can simplify that to just plain dot notation as long as the argument
        -- is an identifier (eg not a complicated expression)
        Application (Identifier (Field key)) [ Literal obj ] ->
            Access (Literal obj) [ Fixed key ]
                |> Just

        -- CONDITIONAL ---------------------------------------------------------
        -- Discard the 'false' branch if the predicate to an 'if .. then .. else'
        -- is the boolean literal `true`.
        Conditional (Literal (Boolean True)) true _ ->
            Just true

        -- Discard the 'true' branch if the predicate to an 'if .. then .. else'
        -- is the boolean literal `false`.
        Conditional (Literal (Boolean False)) _ false ->
            Just false

        -- INFIX ---------------------------------------------------------------
        -- The pipe operator can always be transformed to simple function
        -- application.
        Infix Pipe lhs (Application f args) ->
            Application f (args ++ [ lhs ])
                |> Just

        Infix Pipe lhs (Identifier id) ->
            Application (Identifier id) [ lhs ]
                |> Just

        -- When the left operand of the Add operator is a number, attempt to
        -- coerce the right operand to a number as well and add the two together.
        -- If that fails, fallback to string concatenation instead.
        Infix Add (Literal (Number x)) (Literal y) ->
            let
                addNumbers a b =
                    Maybe.map2 (+) (Just a) (Literal.coerceToNumber b)
                        |> Maybe.map Expression.number

                addStrings a b =
                    Maybe.map2 (++) (Just <| String.fromFloat a) (Literal.coerceToString b)
                        |> Maybe.map Expression.string
            in
            addNumbers x y
                |> (\n ->
                        if n == Nothing then
                            addStrings x y

                        else
                            n
                   )

        -- When the right operand of the Add operator is a number, attempt to
        -- coerce the left operand to a number as well and add the two together.
        -- If that fails, fallback to string concatenation instead.
        Infix Add (Literal x) (Literal (Number y)) ->
            let
                addNumbers a b =
                    Maybe.map2 (+) (Literal.coerceToNumber a) (Just b)
                        |> Maybe.map Expression.number

                addStrings a b =
                    Maybe.map2 (++) (Literal.coerceToString a) (Just <| String.fromFloat b)
                        |> Maybe.map Expression.string
            in
            addNumbers x y
                |> (\n ->
                        if n == Nothing then
                            addStrings x y

                        else
                            n
                   )

        -- When the left operand of the Add operator is anything but a number,
        -- coerce both operands to strings and concatenate them.
        Infix Add (Literal x) (Literal y) ->
            Maybe.map2 (++) (Literal.coerceToString x) (Literal.coerceToString y)
                |> Maybe.map Expression.string

        Infix Sub (Literal x) (Literal y) ->
            Maybe.map2 (-) (Literal.coerceToNumber x) (Literal.coerceToNumber y)
                |> Maybe.map Expression.number

        Infix Mul (Literal x) (Literal y) ->
            Maybe.map2 (*) (Literal.coerceToNumber x) (Literal.coerceToNumber y)
                |> Maybe.map Expression.number

        Infix Div (Literal x) (Literal y) ->
            Maybe.map2 (/) (Literal.coerceToNumber x) (Literal.coerceToNumber y)
                |> Maybe.map Expression.number

        Infix Pow (Literal x) (Literal y) ->
            Maybe.map2 (^) (Literal.coerceToNumber x) (Literal.coerceToNumber y)
                |> Maybe.map Expression.number

        Infix Eq (Literal x) (Literal y) ->
            Just (x == y)
                |> Maybe.map Expression.boolean

        Infix Eq (Identifier a) (Identifier b) ->
            if a == b then
                Expression.boolean True
                    |> Just

            else
                Nothing

        Infix NotEq (Literal x) (Literal y) ->
            Just (x /= y)
                |> Maybe.map Expression.boolean

        Infix NotEq (Identifier a) (Identifier b) ->
            Just (a /= b)
                |> Maybe.map Expression.boolean

        Infix Lt (Literal (Number x)) (Literal (Number y)) ->
            Just (x < y)
                |> Maybe.map Expression.boolean

        Infix Lt (Literal (String a)) (Literal (String b)) ->
            Just (a < b)
                |> Maybe.map Expression.boolean

        Infix Lte (Literal (Number x)) (Literal (Number y)) ->
            Just (x <= y)
                |> Maybe.map Expression.boolean

        Infix Lte (Literal (String a)) (Literal (String b)) ->
            Just (a <= b)
                |> Maybe.map Expression.boolean

        Infix Gt (Literal (Number x)) (Literal (Number y)) ->
            Just (x > y)
                |> Maybe.map Expression.boolean

        Infix Gt (Literal (String a)) (Literal (String b)) ->
            Just (a > b)
                |> Maybe.map Expression.boolean

        Infix Gte (Literal (Number x)) (Literal (Number y)) ->
            Just (x >= y)
                |> Maybe.map Expression.boolean

        Infix Gte (Literal (String a)) (Literal (String b)) ->
            Just (a >= b)
                |> Maybe.map Expression.boolean

        Infix And (Literal (Boolean a)) (Literal (Boolean b)) ->
            Just (a && b)
                |> Maybe.map Expression.boolean

        Infix Or (Literal (Boolean a)) (Literal (Boolean b)) ->
            Just (a || b)
                |> Maybe.map Expression.boolean

        Infix Cons a (Literal (Array elements)) ->
            Just (a :: elements)
                |> Maybe.map Expression.array

        Infix Join (Literal (Array a)) (Literal (Array b)) ->
            Just (a ++ b)
                |> Maybe.map Expression.array

        _ ->
            Nothing



-- OPTIMISATIONS: PARENTHESES STRIPPING ----------------------------------------


{-| This optimisation strips superfluous parentheses from expressions like
`f (a)` or `((1))`.
-}
stripParentheses : Expression -> Maybe Expression
stripParentheses expression =
    case expression of
        -- SUBEXPRESSION -------------------------------------------------------
        -- Unwrap all SubExpressions with single expression contents
        SubExpression (Access expr accessors) ->
            Access expr accessors
                |> Just

        SubExpression (Identifier id) ->
            Identifier id
                |> Just

        SubExpression (Literal literal) ->
            Literal literal
                |> Just

        SubExpression (SubExpression expr) ->
            SubExpression expr
                |> Just


        -- ACCESS --------------------------------------------------------------
        -- Unwrap all SubExpressions inside Computed accessors.
        -- Removes [(expression)] output.
        Access expr accessors ->
            let
                optimiseAccessor acc =
                    case acc of
                        Computed (SubExpression e) ->
                            Computed e
                        _ ->
                            acc
            in
                List.map optimiseAccessor accessors
                    |> (\optimisedAccessors ->
                            if optimisedAccessors == accessors then
                                Nothing
                            else
                                Access expr optimisedAccessors
                                    |> Just
                       )

        -- APPLICATION ---------------------------------------------------------
        -- Unwrap all SubExpressions as arguments.
        -- Prevents arguments being wrappped in double-parentheses.
        Application func arguments ->
            let
                optimiseArgument arg =
                    case arg of
                        SubExpression e ->
                            e
                        _ ->
                            arg
            in
                List.map optimiseArgument arguments
                    |> (\optimisedArguments ->
                            if optimisedArguments == arguments then
                                Nothing
                            else
                                Application func optimisedArguments
                                    |> Just
                       )

        -- CONDITIONAL ---------------------------------------------------------
        -- Unwrap all SubExpressions as condition.
        -- Prevents condition being wrappped in double-parentheses.
        Conditional (SubExpression condition) trueCase falseCase ->
            Conditional condition trueCase falseCase
                |> Just

        -- Unwrap all SubExpressions as body if true.
        -- Prevents true body being wrappped in parentheses.
        Conditional condition (SubExpression trueCase) falseCase ->
            Conditional condition trueCase falseCase
                |> Just

        -- Unwrap all SubExpressions as body if false.
        -- Prevents false body being wrappped in parentheses.
        Conditional condition trueCase (SubExpression falseCase) ->
            Conditional condition trueCase falseCase
                |> Just

        -- LAMBDA --------------------------------------------------------------
        -- Unwrap SubExpression as body.
        -- Prevents body being wrappped in parentheses.
        Lambda patterns (SubExpression body) ->
            Lambda patterns body
             |> Just

        -- MATCH ---------------------------------------------------------------
        -- Unwrap all SubExpressions as arguments.
        -- Prevents arguments being wrappped in double-parentheses.
        Match (SubExpression expr) branches ->
            Match expr branches
                |> Just

        _ ->
            Nothing
