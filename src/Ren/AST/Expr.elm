module Ren.AST.Expr exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Data.Either exposing (Either)
import Data.Tuple2
import Data.Tuple3



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
    | Block (List ( String, expr )) expr
    | Conditional expr expr expr
    | Identifier Identifier
    | Infix Operator expr expr
    | Lambda (List Pattern) expr
    | Literal (Literal expr)
    | Match expr (List ( Pattern, Maybe expr, expr ))


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


annotation : Expr meta -> meta
annotation (Expr meta _) =
    meta



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
map : (a -> b) -> ExprF a -> ExprF b
map f expression =
    case expression of
        Access expr accessors ->
            Access (f expr) accessors

        Application expr args ->
            Application (f expr) (List.map f args)

        Block bindings body ->
            Block (List.map (Tuple.mapSecond f) bindings) (f body)

        Conditional cond true false ->
            Conditional (f cond) (f true) (f false)

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

            Block bindings body ->
                Block (List.map (Tuple.mapSecond (mapAnnotation f)) bindings) (mapAnnotation f body)

            Conditional cond true false ->
                Conditional (mapAnnotation f cond) (mapAnnotation f true) (mapAnnotation f false)

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
