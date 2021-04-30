module Ren.Compiler.Optimise.Expression exposing
    ( optimise
    )


-- IMPORTS ---------------------------------------------------------------------


import Dict
import List.Extra
import Ren.Data.Expression as Expression exposing (Expression(..))
import Ren.Data.Expression.Accessor exposing (Accessor(..))
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

        Comment comment ->
            Comment comment
    
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


-- OPTIMISATIONS: CONSTANT FOLDING ---------------------------------------------


{-| -}
constantFold : Expression -> Maybe Expression
constantFold expression =
    case expression of
        -- ACCESS --------------------------------------------------------------

        -- Attempt to access an array literal by coercing the computed literal
        -- to an integer and then indexing the array with that string.
        Access (Literal (Array elements)) (Computed (Literal n) :: []) ->
            Literal.coerceToInteger n
                |> Maybe.andThen (\i -> List.Extra.at i elements)

        -- Attempt to access an array literal by coercing the computed literal
        -- to an integer and then indexing the array with that string, then turn
        -- that back into a `Access` expression using the remaining accessors.
        Access (Literal (Array elements)) (Computed (Literal n) :: accessors) ->
            Literal.coerceToInteger n
                |> Maybe.andThen (\i -> List.Extra.at i elements)
                |> Maybe.map (\arr -> Access arr accessors)

        -- Attempt to access an object literal with a fixed string key.
        Access (Literal (Object fields)) (Fixed key :: []) ->
            Dict.get key fields

        -- Attempt to access an object literal with a fixed string key, then turn
        -- that back into a `Access` expression using the remaining accessors.
        Access (Literal (Object fields)) (Fixed key :: accessors) ->
            Dict.get key fields
                |> Maybe.map (\obj -> Access obj accessors)

        -- Attempt to access an object literal by coercing the computed literal
        -- to a string and then indexing the object with that string, then turn
        -- that back into a `Access` expression using the remaining accessors.
        Access (Literal (Object fields)) (Computed (Literal s) :: []) ->
            Maybe.map2 Dict.get (Literal.coerceToString s) (Just fields)
                |> Maybe.andThen identity

        -- Attempt to access an object literal by coercing the computed literal
        -- to a string and then indexing the object with that string.
        Access (Literal (Object fields)) (Computed (Literal s) :: accessors) ->
            Maybe.map2 Dict.get (Literal.coerceToString s) (Just fields)
                |> Maybe.andThen identity
                |> Maybe.map (\obj -> Access obj accessors)

        -- CONDITIONAL ---------------------------------------------------------

        -- Discard the 'false' branch if the predicate to an 'if .. then .. else'
        -- is the boolean literal `true`.
        Conditional (Literal (Boolean True)) true _ ->
            Just true

        -- Discard the 'true' branch if the predicate to an 'if .. then .. else'
        -- is the boolean literal `false`.
        Conditional (Literal (Boolean (False))) _ false ->
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

        -- When we're sure the left operand of the discard operator cannot
        -- perform side effects, we can simply remove it from the AST as it must
        -- be a no-op.
        Infix Discard (Comment _) rhs ->
            Just rhs

        Infix Discard (Lambda _ _) rhs ->
            Just rhs

        Infix Discard (Literal (Boolean _)) rhs ->
            Just rhs

        Infix Discard (Literal (Number _)) rhs ->
            Just rhs

        Infix Discard (Literal (String _)) rhs ->
            Just rhs

        Infix Discard (Identifier _) rhs ->
            Just rhs

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
                |> (\n -> if n == Nothing then addStrings x y else n)

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
                |> (\n -> if n == Nothing then addStrings x y else n)

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
            Just (a == b)
                |> Maybe.map Expression.boolean

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
