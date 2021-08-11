module Ren.Compiler.Optimise exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Basics.Extra
import Dict
import List.Extra
import Ren.Language exposing (..)
import Ren.Language.Expression
import Transform



-- OPTIMISING EXPRESSIONS ------------------------------------------------------


{-| -}
optimiseExpression : Expression -> Expression
optimiseExpression =
    Transform.transformAll transformExpression
        (Transform.orList
            [ constantFold
            , removeUnusedInBlock

            -- , stripParentheses
            ]
        )


transformExpression : (Expression -> Expression) -> Expression -> Expression
transformExpression transform expression =
    case expression of
        Access expr accessors ->
            Access
                (transform expr)
                (List.map (transformAccessor transform) accessors)

        Application expr args ->
            Application
                (transform expr)
                (List.map transform args)

        Block bindings expr ->
            Block
                bindings
                (transform expr)

        Conditional condition true false ->
            Conditional
                (transform condition)
                (transform true)
                (transform false)

        Identifier name ->
            Identifier name

        Infix operator lhs rhs ->
            Infix
                operator
                (transform lhs)
                (transform rhs)

        Lambda args body ->
            Lambda
                args
                (transform body)

        Literal literal ->
            Literal
                (transformLiteral transform literal)

        Match expr cases ->
            Match
                (transform expr)
                (List.map (transformCase transform) cases)

        SubExpression expr ->
            SubExpression
                (transform expr)


transformAccessor : (Expression -> Expression) -> Accessor -> Accessor
transformAccessor transform accessor =
    case accessor of
        Computed expr ->
            Computed
                (transform expr)

        Fixed name ->
            Fixed name


transformLiteral : (Expression -> Expression) -> Literal -> Literal
transformLiteral transform literal =
    case literal of
        Array elements ->
            Array
                (List.map transform elements)

        Boolean b ->
            Boolean b

        Number n ->
            Number n

        Object entries ->
            Object
                (Dict.map (always transform) entries)

        String s ->
            String s

        Template segments ->
            Template
                (List.map (transformSegment transform) segments)

        Undefined ->
            Undefined


transformSegment : (Expression -> Expression) -> TemplateSegment -> TemplateSegment
transformSegment transform segment =
    case segment of
        Expr expr ->
            Expr (transform expr)

        Text text ->
            Text text


transformCase : (Expression -> Expression) -> ( Pattern, Maybe Expression, Expression ) -> ( Pattern, Maybe Expression, Expression )
transformCase transform ( pattern, guard, body ) =
    ( pattern
    , Maybe.map transform guard
    , body
    )


constantFold : Expression -> Maybe Expression
constantFold expression =
    let
        coerceToNumber literal =
            case literal of
                Array _ ->
                    Nothing

                Boolean True ->
                    Just 1

                Boolean False ->
                    Just 0

                Number n ->
                    Just n

                Object _ ->
                    Nothing

                String s ->
                    String.toFloat s

                Template _ ->
                    Nothing

                Undefined ->
                    Just 0

        coerceToInteger literal =
            case literal of
                Array _ ->
                    Nothing

                Boolean True ->
                    Just 1

                Boolean False ->
                    Just 0

                Number n ->
                    Basics.Extra.toInt n

                Object _ ->
                    Nothing

                String s ->
                    String.toInt s

                Template _ ->
                    Nothing

                Undefined ->
                    Just 0

        coerceToString literal =
            case literal of
                Array _ ->
                    Nothing

                Boolean True ->
                    Just "true"

                Boolean False ->
                    Just "false"

                Number n ->
                    Just (String.fromFloat n)

                Object _ ->
                    Nothing

                String s ->
                    Just s

                Template segments ->
                    List.foldr
                        (\segment s ->
                            case segment of
                                Expr (Literal lit) ->
                                    Maybe.map2 (++)
                                        (coerceToString lit)
                                        s

                                Expr _ ->
                                    Nothing

                                Text text ->
                                    Maybe.map ((++) text) s
                        )
                        (Just "")
                        segments

                Undefined ->
                    Nothing
    in
    case expression of
        -- ACCESS --------------------------------------------------------------
        -- Attempt to access an array literal by coercing the computed literal
        -- to an integer and then indexing the array with that string.
        Access (Literal (Array elements)) ((Computed (Literal n)) :: []) ->
            coerceToInteger n
                |> Maybe.andThen (\i -> List.Extra.at i elements)

        -- Attempt to access an array literal by coercing the computed literal
        -- to an integer and then indexing the array with that string, then turn
        -- that back into a `Access` expression using the remaining accessors.
        Access (Literal (Array elements)) ((Computed (Literal n)) :: accessors) ->
            coerceToInteger n
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
            Maybe.map2 Dict.get (coerceToString s) (Just fields)
                |> Maybe.andThen identity

        -- Attempt to access an object literal by coercing the computed literal
        -- to a string and then indexing the object with that string.
        Access (Literal (Object fields)) ((Computed (Literal s)) :: accessors) ->
            Maybe.map2 Dict.get (coerceToString s) (Just fields)
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

        -- BLOCK ---------------------------------------------------------------
        Block [] expr ->
            Just expr

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
                    Maybe.map2 (+) (Just a) (coerceToNumber b)
                        |> Maybe.map (Number >> Literal)

                addStrings a b =
                    Maybe.map2 (++) (Just <| String.fromFloat a) (coerceToString b)
                        |> Maybe.map (String >> Literal)
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
                    Maybe.map2 (+) (coerceToNumber a) (Just b)
                        |> Maybe.map (Number >> Literal)

                addStrings a b =
                    Maybe.map2 (++) (coerceToString a) (Just <| String.fromFloat b)
                        |> Maybe.map (String >> Literal)
            in
            addNumbers x y
                |> (\n ->
                        if n == Nothing then
                            addStrings x y

                        else
                            n
                   )

        --
        Infix Add (Literal (Template x)) (Literal (Template y)) ->
            Just (Literal (Template (x ++ y)))

        Infix Add (Literal (String x)) (Literal (Template y)) ->
            Just (Literal (Template (Text x :: y)))

        Infix Add (Literal (Template x)) (Literal (String y)) ->
            Just (Literal (Template (x ++ [ Text y ])))

        -- When the left operand of the Add operator is anything but a number,
        -- coerce both operands to strings and concatenate them.
        Infix Add (Literal x) (Literal y) ->
            Maybe.map2 (++) (coerceToString x) (coerceToString y)
                |> Maybe.map (String >> Literal)

        Infix Sub (Literal x) (Literal y) ->
            Maybe.map2 (-) (coerceToNumber x) (coerceToNumber y)
                |> Maybe.map (Number >> Literal)

        Infix Mul (Literal x) (Literal y) ->
            Maybe.map2 (*) (coerceToNumber x) (coerceToNumber y)
                |> Maybe.map (Number >> Literal)

        Infix Div (Literal x) (Literal y) ->
            Maybe.map2 (/) (coerceToNumber x) (coerceToNumber y)
                |> Maybe.map (Number >> Literal)

        Infix Pow (Literal x) (Literal y) ->
            Maybe.map2 (^) (coerceToNumber x) (coerceToNumber y)
                |> Maybe.map (Number >> Literal)

        Infix Eq (Literal x) (Literal y) ->
            Just (x == y)
                |> Maybe.map (Boolean >> Literal)

        Infix Eq (Identifier a) (Identifier b) ->
            if a == b then
                Boolean True
                    |> Literal
                    |> Just

            else
                Nothing

        Infix NotEq (Literal x) (Literal y) ->
            Just (x /= y)
                |> Maybe.map (Boolean >> Literal)

        Infix NotEq (Identifier a) (Identifier b) ->
            Just (a /= b)
                |> Maybe.map (Boolean >> Literal)

        Infix Lt (Literal (Number x)) (Literal (Number y)) ->
            Just (x < y)
                |> Maybe.map (Boolean >> Literal)

        Infix Lt (Literal (String a)) (Literal (String b)) ->
            Just (a < b)
                |> Maybe.map (Boolean >> Literal)

        Infix Lte (Literal (Number x)) (Literal (Number y)) ->
            Just (x <= y)
                |> Maybe.map (Boolean >> Literal)

        Infix Lte (Literal (String a)) (Literal (String b)) ->
            Just (a <= b)
                |> Maybe.map (Boolean >> Literal)

        Infix Gt (Literal (Number x)) (Literal (Number y)) ->
            Just (x > y)
                |> Maybe.map (Boolean >> Literal)

        Infix Gt (Literal (String a)) (Literal (String b)) ->
            Just (a > b)
                |> Maybe.map (Boolean >> Literal)

        Infix Gte (Literal (Number x)) (Literal (Number y)) ->
            Just (x >= y)
                |> Maybe.map (Boolean >> Literal)

        Infix Gte (Literal (String a)) (Literal (String b)) ->
            Just (a >= b)
                |> Maybe.map (Boolean >> Literal)

        Infix And (Literal (Boolean a)) (Literal (Boolean b)) ->
            Just (a && b)
                |> Maybe.map (Boolean >> Literal)

        Infix Or (Literal (Boolean a)) (Literal (Boolean b)) ->
            Just (a || b)
                |> Maybe.map (Boolean >> Literal)

        Infix Cons a (Literal (Array elements)) ->
            Just (a :: elements)
                |> Maybe.map (Array >> Literal)

        Infix Join (Literal (Array a)) (Literal (Array b)) ->
            Just (a ++ b)
                |> Maybe.map (Array >> Literal)

        _ ->
            Nothing


removeUnusedInBlock : Expression -> Maybe Expression
removeUnusedInBlock expression =
    let
        isPatternUsed expr pattern =
            case pattern of
                ArrayDestructure patterns ->
                    List.any
                        (isPatternUsed expr)
                        patterns

                Name name ->
                    Ren.Language.Expression.references name expr

                ObjectDestructure patterns ->
                    List.any
                        (\( k, p ) ->
                            Maybe.map (isPatternUsed expr) p
                                |> Maybe.map ((||) (Ren.Language.Expression.references k expr))
                                |> Maybe.withDefault False
                        )
                        patterns

                Value _ ->
                    False

                VariantDestructure _ patterns ->
                    List.any
                        (isPatternUsed expr)
                        patterns

                -- Wildcard match can never be safely eliminated because it might
                -- be used to perform side effects.
                Wildcard _ ->
                    True

        isBindingUsed expr binding =
            case binding of
                Function name _ _ ->
                    Ren.Language.Expression.references name expr

                Variable name _ ->
                    isPatternUsed expr name

                Enum _ variants ->
                    List.any
                        (\(Variant tag _) ->
                            Ren.Language.Expression.references tag expr
                        )
                        variants
    in
    case expression of
        Block bindings expr ->
            let
                filteredBindings =
                    List.filter (isBindingUsed expr) bindings
            in
            if filteredBindings == bindings then
                Nothing

            else
                Just (Block filteredBindings expr)

        _ ->
            Nothing
