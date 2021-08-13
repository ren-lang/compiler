module Ren.Compiler.Optimise exposing
    ( optimiseModule, optimiseModuleWith, moduleDefaults
    , optimiseDeclaration, optimiseDeclarationWith
    , optimiseExpression, optimiseExpressionWith, expressionDefaults
    )

{-|

@docs optimiseModule, optimiseModuleWith, moduleDefaults
@docs optimiseDeclaration, optimiseDeclarationWith
@docs optimiseExpression, optimiseExpressionWith, expressionDefaults

-}

-- IMPORTS ---------------------------------------------------------------------

import Dict
import List.Extra
import Maybe.Extra
import Ren.Language exposing (..)
import Ren.Language.Declaration
import Ren.Language.Expression
import Ren.Language.Pattern
import Transform



-- OPTIMISING MODULES ----------------------------------------------------------


{-| -}
optimiseModuleWith : List (Module -> Maybe Module) -> Module -> Module
optimiseModuleWith =
    let
        apply : List (Module -> Maybe Module) -> Module -> Module
        apply optimisations module_ =
            case optimisations of
                _ :: _ ->
                    module_
                        |> List.foldl or (always Nothing) optimisations
                        |> Maybe.map (apply optimisations)
                        |> Maybe.withDefault module_

                [] ->
                    module_

        or : (Module -> Maybe Module) -> (Module -> Maybe Module) -> Module -> Maybe Module
        or f g m =
            f m |> Maybe.map Just |> Maybe.withDefault (g m)
    in
    apply


{-| -}
optimiseModule : Module -> Module
optimiseModule =
    optimiseModuleWith moduleDefaults


{-| -}
moduleDefaults : List (Module -> Maybe Module)
moduleDefaults =
    [ simplifyDeclarations
    , removeUnusedDeclarations
    , removeUnusedImports
    , removeUnusedImportBindings
    ]


simplifyDeclarations : Module -> Maybe Module
simplifyDeclarations m =
    List.map (Tuple.mapSecond optimiseDeclaration) m.declarations
        |> (\declarations ->
                if declarations == m.declarations then
                    Nothing

                else
                    Just { m | declarations = declarations }
           )


removeUnusedDeclarations : Module -> Maybe Module
removeUnusedDeclarations m =
    List.filter (isDeclarationUsed m.declarations) m.declarations
        |> (\declarations ->
                if declarations == m.declarations then
                    Nothing

                else
                    Just { m | declarations = declarations }
           )


isDeclarationUsed : List ( Visibility, Declaration ) -> ( Visibility, Declaration ) -> Bool
isDeclarationUsed declarations ( visibility, declaration ) =
    if visibility == Public then
        True

    else
        let
            name =
                Ren.Language.Declaration.nameAsPattern declaration

            names =
                Ren.Language.Pattern.names name

            referencesAny d =
                List.any (\n -> Ren.Language.Declaration.references n d) names
        in
        declarations
            |> List.filter (Tuple.second >> Ren.Language.Declaration.nameAsPattern >> (/=) name)
            |> List.any (Tuple.second >> referencesAny)


removeUnusedImports : Module -> Maybe Module
removeUnusedImports m =
    List.filter (isImportUsed m.declarations) m.imports
        |> (\imports ->
                if imports == m.imports then
                    Nothing

                else
                    Just { m | imports = imports }
           )


isImportUsed : List ( Visibility, Declaration ) -> Import -> Bool
isImportUsed declarations { name, bindings } =
    List.any
        (\( _, declaration ) ->
            Ren.Language.Declaration.referencesNamespace name declaration
                || List.any (\binding -> Ren.Language.Declaration.references binding declaration) bindings
        )
        declarations


removeUnusedImportBindings : Module -> Maybe Module
removeUnusedImportBindings m =
    List.map (removeUnusedBindings m.declarations) m.imports
        |> (\imports ->
                if imports == m.imports then
                    Nothing

                else
                    Just { m | imports = imports }
           )


removeUnusedBindings : List ( Visibility, Declaration ) -> Import -> Import
removeUnusedBindings declarations { path, name, bindings } =
    { path = path
    , name = name
    , bindings =
        List.filter
            (\binding ->
                List.any
                    (\( _, declaration ) ->
                        Ren.Language.Declaration.references binding declaration
                    )
                    declarations
            )
            bindings
    }



-- OPTIMISING DECLARATIONS -----------------------------------------------------


{-| -}
optimiseDeclarationWith : List (Expression -> Maybe Expression) -> Declaration -> Declaration
optimiseDeclarationWith optimisations declaration =
    case declaration of
        Function name args body ->
            Function name args (optimiseExpressionWith optimisations body)

        Variable name body ->
            Variable name (optimiseExpressionWith optimisations body)

        Enum name variants ->
            Enum name variants


{-| -}
optimiseDeclaration : Declaration -> Declaration
optimiseDeclaration =
    optimiseDeclarationWith expressionDefaults



-- OPTIMISING EXPRESSIONS ------------------------------------------------------


{-| -}
optimiseExpressionWith : List (Expression -> Maybe Expression) -> Expression -> Expression
optimiseExpressionWith optimisations =
    Transform.transformAll transformExpression
        (Transform.orList optimisations)


{-| -}
optimiseExpression : Expression -> Expression
optimiseExpression =
    optimiseExpressionWith expressionDefaults


{-| -}
expressionDefaults : List (Expression -> Maybe Expression)
expressionDefaults =
    [ constantFold
    , stripParentheses
    ]


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
    case expression of
        -- ACCESS --------------------------------------------------------------
        -- Attempt to access an array literal by coercing the computed literal
        -- to an integer and then indexing the array with that string.
        Access (Literal (Array elements)) ((Computed (Literal n)) :: []) ->
            Ren.Language.Expression.coerceToInteger n
                |> Maybe.andThen (\i -> List.Extra.at i elements)

        -- Attempt to access an array literal by coercing the computed literal
        -- to an integer and then indexing the array with that string, then turn
        -- that back into a `Access` expression using the remaining accessors.
        Access (Literal (Array elements)) ((Computed (Literal n)) :: accessors) ->
            Ren.Language.Expression.coerceToInteger n
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
            Maybe.map2 Dict.get (Ren.Language.Expression.coerceToString s) (Just fields)
                |> Maybe.andThen identity

        -- Attempt to access an object literal by coercing the computed literal
        -- to a string and then indexing the object with that string.
        Access (Literal (Object fields)) ((Computed (Literal s)) :: accessors) ->
            Maybe.map2 Dict.get (Ren.Language.Expression.coerceToString s) (Just fields)
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
        -- Ren.Language.Expression.coerce the right operand to a number as well and add the two together.
        -- If that fails, fallback to string concatenation instead.
        Infix Add (Literal (Number x)) (Literal y) ->
            let
                addNumbers a b =
                    Maybe.map2 (+) (Just a) (Ren.Language.Expression.coerceToNumber b)
                        |> Maybe.map (Number >> Literal)

                addStrings a b =
                    Maybe.map2 (++) (Just <| String.fromFloat a) (Ren.Language.Expression.coerceToString b)
                        |> Maybe.map (String >> Literal)
            in
            addNumbers x y
                |> Maybe.Extra.or (addStrings x y)

        -- When the right operand of the Add operator is a number, attempt to
        -- Ren.Language.Expression.coerce the left operand to a number as well and add the two together.
        -- If that fails, fallback to string concatenation instead.
        Infix Add (Literal x) (Literal (Number y)) ->
            let
                addNumbers a b =
                    Maybe.map2 (+) (Ren.Language.Expression.coerceToNumber a) (Just b)
                        |> Maybe.map (Number >> Literal)

                addStrings a b =
                    Maybe.map2 (++) (Ren.Language.Expression.coerceToString a) (Just <| String.fromFloat b)
                        |> Maybe.map (String >> Literal)
            in
            addNumbers x y
                |> Maybe.Extra.or (addStrings x y)

        --
        Infix Add (Literal (Template x)) (Literal (Template y)) ->
            Just (Literal (Template (x ++ y)))

        Infix Add (Literal (String x)) (Literal (Template y)) ->
            Just (Literal (Template (Text x :: y)))

        Infix Add (Literal (Template x)) (Literal (String y)) ->
            Just (Literal (Template (x ++ [ Text y ])))

        -- When the left operand of the Add operator is anything but a number,
        -- Ren.Language.Expression.coerce both operands to strings and concatenate them.
        Infix Add (Literal x) (Literal y) ->
            Maybe.map2 (++) (Ren.Language.Expression.coerceToString x) (Ren.Language.Expression.coerceToString y)
                |> Maybe.map (String >> Literal)

        Infix Sub (Literal x) (Literal y) ->
            Maybe.map2 (-) (Ren.Language.Expression.coerceToNumber x) (Ren.Language.Expression.coerceToNumber y)
                |> Maybe.map (Number >> Literal)

        Infix Mul (Literal x) (Literal y) ->
            Maybe.map2 (*) (Ren.Language.Expression.coerceToNumber x) (Ren.Language.Expression.coerceToNumber y)
                |> Maybe.map (Number >> Literal)

        Infix Div (Literal x) (Literal y) ->
            Maybe.map2 (/) (Ren.Language.Expression.coerceToNumber x) (Ren.Language.Expression.coerceToNumber y)
                |> Maybe.map (Number >> Literal)

        Infix Pow (Literal x) (Literal y) ->
            Maybe.map2 (^) (Ren.Language.Expression.coerceToNumber x) (Ren.Language.Expression.coerceToNumber y)
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
