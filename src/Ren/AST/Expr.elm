module Ren.AST.Expr exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Data.Either exposing (Either)
import Data.Tuple2
import Data.Tuple3
import Ren.Data.Type as Type exposing (Type)



-- TYPES -----------------------------------------------------------------------


{-| Represents an expression node in an AST with some accompanying metadata. We
might have `Expr Type` to represent a typed expression, or `Expr Span` to know
where an expression exists in the source code.
-}
type Expr meta
    = Expr meta (ExprF (Expr meta))


{-| -}
type ExprF expr
    = Access expr (List String)
    | Application expr (List expr)
    | Annotation expr Type
    | Block (List ( String, expr )) expr
    | Conditional expr expr expr
    | Error (Maybe Error)
    | Identifier Identifier
    | Infix Operator expr expr
    | Lambda (List Pattern) expr
    | Literal (Literal expr)
    | Match expr (List ( Pattern, Maybe expr, expr ))


{-| -}
type Error
    = MissingElement
    | MissingSymbol String
    | UnexpectedSymbol String


{-| -}
type Identifier
    = Local String
    | Scoped String Identifier
    | Placeholder (Maybe String)


{-| -}
type Literal expr
    = Array (List expr)
    | Boolean Bool
    | Number Float
      --
      -- Why isn't this a Dict? While its true that object keys should be unique,
      -- that isn't the ASTs job! We have a separate verification step to determine
      -- this.
    | Record (List ( String, expr ))
    | String String
    | Template (List (Either String expr))
    | Undefined
    | Variant String (List expr)


{-| -}
type Operator
    = Pipe
    | Compose
      -- MATHS
    | Add
    | Sub
    | Mul
    | Div
    | Pow
    | Mod
      -- COMPARISON
    | Eq
    | NotEq
    | Lt
    | Lte
    | Gt
    | Gte
      -- LOGIC
    | And
    | Or
      -- ARRAYS
    | Cons
    | Join


{-| -}
type Pattern
    = ArrayDestructure (List Pattern)
    | LiteralPattern (Literal Never)
    | Name String
    | RecordDestructure (List ( String, Maybe Pattern ))
    | Spread String
    | Typeof String Pattern
    | VariantDestructure String (List Pattern)
    | Wildcard (Maybe String)



-- QUERIES ---------------------------------------------------------------------


{-| -}
annotation : Expr meta -> meta
annotation (Expr meta _) =
    meta



-- QUERIES ---------------------------------------------------------------------


{-| -}
references : Identifier -> Expr meta -> Bool
references identifier expr =
    let
        localNameString =
            case identifier of
                Local name ->
                    Just name

                _ ->
                    Nothing
    in
    cata
        (\_ exprF ->
            case exprF of
                Access referencedInExpr _ ->
                    referencedInExpr

                Application referencedInExpr referencedInArgs ->
                    referencedInExpr || List.any Basics.identity referencedInArgs

                Annotation referencedInExpr _ ->
                    referencedInExpr

                Block bindings referencedInBody ->
                    let
                        shadowed ( binding, _ ) =
                            Just binding == localNameString
                    in
                    List.all (Basics.not << shadowed) bindings
                        && (referencedInBody || List.any Tuple.second bindings)

                Conditional referencedInCond referencedInTrue referencedInFalse ->
                    referencedInCond || referencedInTrue || referencedInFalse

                Error _ ->
                    False

                Identifier id ->
                    identifier == id

                Infix _ referencedInLHS referencedInRHS ->
                    referencedInLHS || referencedInRHS

                Lambda args referencedInBody ->
                    let
                        shadowed pattern =
                            Maybe.map2 binds localNameString (Just pattern)
                                |> Maybe.withDefault False
                    in
                    List.all (Basics.not << shadowed) args && referencedInBody

                Literal (Array referencedInElements) ->
                    List.any Basics.identity referencedInElements

                Literal (Boolean _) ->
                    False

                Literal (Number _) ->
                    False

                Literal (Record referencedInEntries) ->
                    List.any Tuple.second referencedInEntries

                Literal (String _) ->
                    False

                Literal (Template referencedInSegments) ->
                    List.any (Data.Either.extract (always False) Basics.identity) referencedInSegments

                Literal Undefined ->
                    False

                Literal (Variant _ referencedInArgs) ->
                    List.any Basics.identity referencedInArgs

                Match referencedInExpr referencedInCases ->
                    let
                        referencedInCase ( pattern, guard, referencedInBody ) =
                            Basics.not (shadowed pattern) && (referencedInBody || referencedInGuard guard)

                        shadowed pattern =
                            Maybe.map2 binds localNameString (Just pattern)
                                |> Maybe.withDefault False

                        referencedInGuard guard =
                            Maybe.withDefault False guard
                    in
                    referencedInExpr || List.any referencedInCase referencedInCases
        )
        expr


{-| -}
shadows : Identifier -> Expr meta -> Bool
shadows identifier expr =
    case identifier of
        Local name ->
            cata
                (\_ exprF ->
                    case exprF of
                        Access shadowedInExpr _ ->
                            shadowedInExpr

                        Application shadowedInExpr shadowedInArgs ->
                            shadowedInExpr || List.any Basics.identity shadowedInArgs

                        Annotation shadowedInExpr _ ->
                            shadowedInExpr

                        Block shadowedInBindings shadowedInBody ->
                            List.any (Tuple.first >> (==) name) shadowedInBindings
                                || List.any Tuple.second shadowedInBindings
                                || shadowedInBody

                        Conditional shadowedInCond shadowedInTrue shadowedInFalse ->
                            shadowedInCond || shadowedInTrue || shadowedInFalse

                        Error _ ->
                            False

                        Identifier _ ->
                            False

                        Infix _ shadowedInLHS shadowedInRHS ->
                            shadowedInLHS || shadowedInRHS

                        Lambda args shadowedInBody ->
                            List.any (binds name) args || shadowedInBody

                        Literal (Array shadowedInElements) ->
                            List.any Basics.identity shadowedInElements

                        Literal (Boolean _) ->
                            False

                        Literal (Number _) ->
                            False

                        Literal (Record shadowedInEntries) ->
                            List.any Tuple.second shadowedInEntries

                        Literal (String _) ->
                            False

                        Literal (Template shadowedInSegments) ->
                            List.any (Data.Either.extract (always False) Basics.identity) shadowedInSegments

                        Literal Undefined ->
                            False

                        Literal (Variant _ shadowedInArgs) ->
                            List.any Basics.identity shadowedInArgs

                        Match shadowedInExpr shadowedInCases ->
                            shadowedInExpr
                                || List.any
                                    (\( pattern, shadowedInGuard, shadowedInBody ) ->
                                        binds name pattern
                                            || Maybe.withDefault False shadowedInGuard
                                            || shadowedInBody
                                    )
                                    shadowedInCases
                )
                expr

        -- It's impossible to shadow a scoped or placeholder identifier. This
        -- function accepts `Identifier`s rather than `String`s so maintain a
        -- consistent API with `references`.
        _ ->
            False


{-| -}
bound : Pattern -> List String
bound pattern =
    case pattern of
        ArrayDestructure patterns ->
            List.concatMap bound patterns

        LiteralPattern _ ->
            []

        Name n ->
            [ n ]

        RecordDestructure entries ->
            List.concatMap (\( k, p ) -> Maybe.map bound p |> Maybe.withDefault [ k ]) entries

        Spread n ->
            [ n ]

        Typeof _ pat ->
            bound pat

        VariantDestructure _ patterns ->
            List.concatMap bound patterns

        Wildcard _ ->
            []


{-| Checks to see if a Pattern introduces a new binding with a name that matches
the argument. This is necessary in, for example, the `references` check because
a binding may shadow the name we're checking is referenced and we don't want a
false positive.
-}
binds : String -> Pattern -> Bool
binds name pattern =
    case pattern of
        ArrayDestructure patterns ->
            List.any (binds name) patterns

        LiteralPattern _ ->
            False

        Name n ->
            name == n

        RecordDestructure entries ->
            List.any (\( k, p ) -> Maybe.map (binds name) p |> Maybe.withDefault (k == name)) entries

        Spread n ->
            name == n

        Typeof _ pat ->
            binds name pat

        VariantDestructure _ patterns ->
            List.any (binds name) patterns

        Wildcard _ ->
            False



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
map : (a -> b) -> ExprF a -> ExprF b
map f expression =
    case expression of
        Access expr accessors ->
            Access (f expr) accessors

        Application expr args ->
            Application (f expr) (List.map f args)

        Annotation expr t ->
            Annotation (f expr) t

        Block bindings body ->
            Block (List.map (Tuple.mapSecond f) bindings) (f body)

        Conditional cond true false ->
            Conditional (f cond) (f true) (f false)

        Error e ->
            Error e

        Identifier id ->
            Identifier id

        Infix op lhs rhs ->
            Infix op (f lhs) (f rhs)

        Lambda args body ->
            Lambda args (f body)

        Literal (Array entries) ->
            Literal (Array (List.map f entries))

        Literal (Boolean b) ->
            Literal (Boolean b)

        Literal (Number n) ->
            Literal (Number n)

        Literal (Record entries) ->
            Literal (Record (List.map (Tuple.mapSecond f) entries))

        Literal (String s) ->
            Literal (String s)

        Literal (Template segments) ->
            Literal (Template (List.map (Data.Either.mapBoth identity f) segments))

        Literal Undefined ->
            Literal Undefined

        Literal (Variant tag args) ->
            Literal (Variant tag (List.map f args))

        Match expr cases ->
            Match (f expr) (List.map (Data.Tuple3.mapAll identity (Maybe.map f) f) cases)


{-| -}
mapAnnotation : (a -> b) -> Expr a -> Expr b
mapAnnotation f (Expr a expression) =
    Expr (f a) <|
        case expression of
            Access expr accessors ->
                Access (mapAnnotation f expr) accessors

            Application expr args ->
                Application (mapAnnotation f expr) (List.map (mapAnnotation f) args)

            Annotation expr t ->
                Annotation (mapAnnotation f expr) t

            Block bindings body ->
                Block (List.map (Tuple.mapSecond (mapAnnotation f)) bindings) (mapAnnotation f body)

            Conditional cond true false ->
                Conditional (mapAnnotation f cond) (mapAnnotation f true) (mapAnnotation f false)

            Error e ->
                Error e

            Identifier id ->
                Identifier id

            Infix op lhs rhs ->
                Infix op (mapAnnotation f lhs) (mapAnnotation f rhs)

            Lambda args body ->
                Lambda args (mapAnnotation f body)

            Literal (Array entries) ->
                Literal (Array (List.map (mapAnnotation f) entries))

            Literal (Boolean b) ->
                Literal (Boolean b)

            Literal (Number n) ->
                Literal (Number n)

            Literal (Record entries) ->
                Literal (Record (List.map (Tuple.mapSecond (mapAnnotation f)) entries))

            Literal (String s) ->
                Literal (String s)

            Literal (Template segments) ->
                Literal (Template (List.map (Data.Either.mapBoth identity (mapAnnotation f)) segments))

            Literal Undefined ->
                Literal Undefined

            Literal (Variant tag args) ->
                Literal (Variant tag (List.map (mapAnnotation f) args))

            Match expr cases ->
                Match (mapAnnotation f expr) (List.map (Data.Tuple3.mapAll identity (Maybe.map (mapAnnotation f)) (mapAnnotation f)) cases)


{-| -}
erase : Expr a -> Expr ()
erase =
    mapAnnotation (always ())



-- RECURSION SCHEMES -----------------------------------------------------------


{-| -}
unwrap : Expr meta -> ExprF (Expr meta)
unwrap (Expr _ expression) =
    expression


{-| -}
wrap : meta -> ExprF (Expr meta) -> Expr meta
wrap meta expression =
    Expr meta expression


{-| -}
cata : (meta -> ExprF a -> a) -> Expr meta -> a
cata f (Expr meta expression) =
    expression |> map (cata f) |> f meta


{-| -}
para : (ExprF ( Expr meta, a ) -> a) -> Expr meta -> a
para f a =
    unwrap a |> map (Data.Tuple2.fromBy Basics.identity (para f)) |> f



-- CONVERSIONS -----------------------------------------------------------------


{-| -}
coerceToNumber : ExprF a -> Maybe Float
coerceToNumber expr =
    case expr of
        Literal (Boolean True) ->
            Just 1

        Literal (Boolean False) ->
            Just 0

        Literal (Number n) ->
            Just n

        Literal (String s) ->
            String.toFloat s

        Literal Undefined ->
            Just 0

        _ ->
            Nothing


{-| -}
coerceToBoolean : ExprF a -> Maybe Bool
coerceToBoolean expr =
    case expr of
        Literal (Boolean b) ->
            Just b

        Literal (Number n) ->
            if n == 0 then
                Just False

            else
                Just True

        Literal (String s) ->
            String.toFloat s
                |> Maybe.map (Number >> Literal)
                |> Maybe.andThen coerceToBoolean

        Literal Undefined ->
            Just False

        _ ->
            Nothing


internalOperatorName : Operator -> String
internalOperatorName op =
    case op of
        Pipe ->
            "$op_pipe"

        Compose ->
            "$op_compose"

        Add ->
            "$op_add"

        Sub ->
            "$op_sub"

        Mul ->
            "$op_mul"

        Div ->
            "$op_div"

        Pow ->
            "$op_pow"

        Mod ->
            "$op_mod"

        Eq ->
            "$op_eq"

        NotEq ->
            "$op_notEq"

        Lt ->
            "$op_lt"

        Lte ->
            "$op_lte"

        Gt ->
            "$op_gt"

        Gte ->
            "$op_gte"

        And ->
            "$op_and"

        Or ->
            "$op_or"

        Cons ->
            "$op_cons"

        Join ->
            "$op_join"
