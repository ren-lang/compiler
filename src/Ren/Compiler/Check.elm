module Ren.Compiler.Check exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Control.ResultM as ResultM exposing (ResultM, do)
import Data.Either
import Data.Tuple2
import Dict
import Ren.AST.Expr as Expr exposing (Expr(..), ExprF(..))
import Ren.AST.Module as Module
import Ren.Data.Monoenv as Monoenv exposing (Monoenv)
import Ren.Data.Polyenv exposing (Polyenv)
import Ren.Data.Substitution as Substitution
import Ren.Data.Type as Type exposing (..)
import Ren.Data.Typing as Typing exposing (Typing)
import Set



-- TYPES -----------------------------------------------------------------------


{-| The `InferM` monad (and by extension, the `ResultM` monad) is where a lot of
the magic happens. It is actually just a type alias for the following function:

    Context -> ( Context, Result Error a )

but functions are monadic, and we can chain them! By doing so we can thread state
through our type inferrence algorithm and stil lean on `Result` on our error
handling.

One function you will see used frequently here is `do`, which is like `ResultM.andThen`
with the arguments flipped. Combined with the hacky elm-format wrapper found in
scripts/elm-format.js we have a crude imitation of Haskell-style do notation where
we perform a monadic action and bind its result to be used later.

-}
type alias InferM a =
    ResultM Context Error a


{-| The context is the state we thread through our inference algorithm.
-}
type alias Context =
    { polyenv : Polyenv
    , substitution : Substitution
    , vars : List String
    }


{-| -}
type Error
    = InternalError String
    | InfiniteType
    | IncompatibleTypes Type Type
    | ContradictingConstraints
    | MissingField String
    | TypeTooGeneral Type Type



--


{-| -}
declaration : Polyenv -> Module.Declaration meta -> Result Error (Module.Declaration meta)
declaration polyenv ({ type_, expr } as declr) =
    if type_ == Type.Any then
        Ok declr

    else
        Result.andThen
            (\( envDec, tDec ) ->
                ResultM.runM init <|
                    do (inst <| Typing.poly type_) <| \( envAnn, tAnn ) ->
                    do (unify [ envDec, envAnn ] [ tDec, tAnn ]) <| \t ->
                    -- There has to be a better way to do this. This is checking
                    -- if the annotated type is too general, for example if we
                    -- have the following function:
                    --
                    -- let id : a -> a =
                    --   a => 10
                    --
                    -- Then the annotated type `a` is too general for the actual
                    -- type `Number`. If we just try and unify the two, though,
                    -- it would succeed because we would produce a substitution
                    -- `a ↦ Number`.
                    --
                    -- To make sure the type annotation isn't more general than it
                    -- should be this is a very crude check to confirm that the
                    -- number of free variables is the same in both the inferred
                    -- type and the annotated one.
                    if Set.size (Typing.free t) /= Set.size (Type.free type_) then
                        ResultM.fail <| TypeTooGeneral type_ (Typing.type_ t)

                    else
                        ResultM.succeed declr
            )
            (expression polyenv expr)


{-| Infer the type of an expression, taking in some existing polymorphic environment
and merging it with the `init` environment that includes typings for builtin
operators and functions.

---

TODO: Ultimately, this returns just the typing for the expression passed in
and _without_ annotating the subexpressions with their respective typings as well.
This is fine for type checking declarations but it's not going to be very useful
if we want tooling to interface with the compiler, or if we want a nice playground
experience.

We should look at using `Expr.para` instead of cata, or perhaps another recursion
scheme to annotate the expressions as we go.

-}
expression : Polyenv -> Expr meta -> Result Error Typing
expression polyenv expr =
    Expr.cata (always infer) expr
        |> ResultM.runM { init | polyenv = Dict.union init.polyenv polyenv }



--


{-| The initial context contains the polymorphic typings for the various built-in
operators, a (very large) list of variables we can continually pluck from, and
an initially empty substitution.
-}
init : Context
init =
    { polyenv =
        Dict.fromList
            [ ( "$op_pipe", Typing.poly (fun (Var "a") [ fun (Var "a") [] (Var "b") ] (Var "b")) )
            , ( "$op_compose", Typing.poly (fun (fun (Var "a") [] (Var "b")) [ fun (Var "b") [] (Var "c") ] (fun (Var "a") [] (Var "c"))) )
            , ( "$op_add", Typing.poly (fun number [ number ] number) )
            , ( "$op_sub", Typing.poly (fun number [ number ] number) )
            , ( "$op_mul", Typing.poly (fun number [ number ] number) )
            , ( "$op_div", Typing.poly (fun number [ number ] number) )
            , ( "$op_pow", Typing.poly (fun number [ number ] number) )
            , ( "$op_mod", Typing.poly (fun number [ number ] number) )
            , ( "$op_eq", Typing.poly (fun (Var "a") [ Var "a" ] boolean) )
            , ( "$op_notEq", Typing.poly (fun (Var "a") [ Var "a" ] boolean) )
            , ( "$op_lt", Typing.poly (fun number [ number ] boolean) )
            , ( "$op_lte", Typing.poly (fun number [ number ] boolean) )
            , ( "$op_gt", Typing.poly (fun number [ number ] boolean) )
            , ( "$op_gte", Typing.poly (fun number [ number ] boolean) )
            , ( "$op_and", Typing.poly (fun boolean [ boolean ] boolean) )
            , ( "$op_or", Typing.poly (fun boolean [ boolean ] boolean) )
            , ( "$op_cons", Typing.poly (fun (Var "a") [ array (Var "a") ] (array (Var "a"))) )
            , ( "$op_join", Typing.poly (fun (array (Var "a")) [ array (Var "a") ] (array (Var "a"))) )
            ]
    , substitution = Dict.empty
    , vars = List.range 0 (25 * 26 + 26) |> List.map Type.var
    }



--                                                                            --
-- INFERRING EXPRESSIONS -------------------------------------------------------
--                                                                            --


{-| -}
infer : ExprF (InferM Typing) -> InferM Typing
infer exprF =
    case exprF of
        Access expr accessors ->
            Debug.todo ""

        Application expr args ->
            application expr args

        Block bindings expr ->
            Debug.todo ""

        Conditional cond true false ->
            conditional cond true false

        Identifier id ->
            identifier id

        Infix op lhs rhs ->
            infix_ op lhs rhs

        Lambda args expr ->
            lambda args expr

        Literal lit ->
            literal lit

        Match expr cases ->
            Debug.todo ""



-- INFERRING EXPRESSIONS: APPLICATION ------------------------------------------


{-| -}
application : InferM Typing -> List (InferM Typing) -> InferM Typing
application inferFun inferArgs =
    List.foldl (\a f -> application_ f a) inferFun inferArgs


{-| -}
application_ : InferM Typing -> InferM Typing -> InferM Typing
application_ inferFun inferArg =
    do inferFun <| \( envFun, tFun ) ->
    do inferArg <| \( envArg, tArg ) ->
    do next <| \a ->
    do (unify [ envFun, envArg ] [ tFun, Fun tArg a ]) <| \( env, t ) ->
    case t of
        Fun _ r ->
            do getSubstitution <| \s ->
            ResultM.succeed <| Typing.substitute s ( env, r )

        _ ->
            do next <| \b ->
            do getSubstitution <| \s ->
            ResultM.succeed <| Typing.substitute s ( env, b )



-- INFERRING EXPRESSIONS: CONDITIONALS -----------------------------------------


{-| -}
conditional : InferM Typing -> InferM Typing -> InferM Typing -> InferM Typing
conditional inferCond inferTrue inferFalse =
    do inferCond <| \cond ->
    do inferTrue <| \true ->
    do inferFalse <| \false ->
    do (unify [ Typing.env cond ] [ Typing.type_ cond, Type.boolean ]) <| \t ->
    unify
        [ Typing.env t, Typing.env true, Typing.env false ]
        [ Typing.type_ true, Typing.type_ false ]



-- INFERRING EXPRESSIONS: IDENTIFIERS ------------------------------------------


identifier : Expr.Identifier -> InferM Typing
identifier id =
    case id of
        Expr.Local name ->
            do (lookup name) <| \t ->
            case t of
                Just typing ->
                    inst typing

                Nothing ->
                    do next <| \a ->
                    ResultM.succeed (Typing.mono name a)

        Expr.Scoped _ _ ->
            Debug.todo ""

        Expr.Placeholder _ ->
            Debug.todo ""



-- INFERRING EXPRESSIONS: INFIX OPERATORS --------------------------------------


{-| -}
infix_ : Expr.Operator -> InferM Typing -> InferM Typing -> InferM Typing
infix_ op lhs rhs =
    application
        (lookupBuiltin <| Expr.internalOperatorName op)
        [ lhs, rhs ]



-- INFERRING EXPRESSIONS: LAMBDAS ----------------------------------------------


{-| -}
lambda : List Expr.Pattern -> InferM Typing -> InferM Typing
lambda patterns inferExpr =
    List.foldl (lambda_ << Basics.identity) inferExpr patterns


{-| -}
lambda_ : Expr.Pattern -> InferM Typing -> InferM Typing
lambda_ pat inferExpr =
    do (pattern pat) <| \( envP, tP ) ->
    do inferExpr <| \( envE, tE ) ->
    do (unify [ envP, envE ] [ Fun tP tE ]) <| \( env, t ) ->
    let
        removePolyvar v e =
            case Monoenv.get v e of
                Just (Var _) ->
                    Monoenv.remove v e

                _ ->
                    e

        preserveLetPolymorphism e =
            Expr.bound pat |> List.foldl removePolyvar e
    in
    ResultM.succeed <| Typing.from (preserveLetPolymorphism env) t



-- INFERRING EXPRESSIONS: LITERALS ---------------------------------------------


literal : Expr.Literal (InferM Typing) -> InferM Typing
literal lit =
    case lit of
        Expr.Array inferElements ->
            do (ResultM.map List.unzip <| ResultM.sequence inferElements) <| \( envs, ts ) ->
            do (unify envs ts) <| \( env, t ) ->
            ResultM.succeed <| Typing.from env (array t)

        Expr.Boolean _ ->
            ResultM.succeed <| Typing.poly (Con "Boolean")

        Expr.Number _ ->
            ResultM.succeed <| Typing.poly (Con "Number")

        Expr.Record entries ->
            Debug.todo ""

        Expr.String _ ->
            ResultM.succeed <| Typing.poly (Con "String")

        Expr.Template inferSegments ->
            inferSegments
                |> List.map (Data.Either.extract (always <| ResultM.succeed (Typing.poly Type.string)) Basics.identity)
                |> ResultM.sequence
                |> ResultM.map List.unzip
                |> ResultM.andThen (Data.Tuple2.apply unify)

        Expr.Undefined ->
            ResultM.succeed <| Typing.poly (Con "()")

        Expr.Variant tag args ->
            Debug.todo ""



-- INFERRING EXPRESSIONS: PATTERNS ---------------------------------------------


pattern : Expr.Pattern -> InferM Typing
pattern p =
    case p of
        Expr.ArrayDestructure patterns ->
            do (ResultM.map List.unzip <| ResultM.sequence <| List.map pattern patterns) <| \( envs, ts ) ->
            do (unify envs ts) <| \( env, t ) ->
            ResultM.succeed <| Typing.from env (array t)

        Expr.LiteralPattern (Expr.Array _) ->
            ResultM.fail <| InternalError "Cannot infer type of array literal pattern."

        Expr.LiteralPattern (Expr.Boolean _) ->
            ResultM.succeed <| Typing.poly (Con "Boolean")

        Expr.LiteralPattern (Expr.Number _) ->
            ResultM.succeed <| Typing.poly (Con "Number")

        Expr.LiteralPattern (Expr.Record _) ->
            ResultM.fail <| InternalError "Cannot infer type of record literal pattern."

        Expr.LiteralPattern (Expr.String _) ->
            ResultM.succeed <| Typing.poly (Con "String")

        Expr.LiteralPattern (Expr.Template _) ->
            ResultM.fail <| InternalError "Cannot infer type of template literal pattern."

        Expr.LiteralPattern Expr.Undefined ->
            ResultM.succeed <| Typing.poly (Con "()")

        Expr.LiteralPattern (Expr.Variant _ _) ->
            ResultM.fail <| InternalError "Cannot infer type of variant literal pattern."

        Expr.Name name ->
            do next <| \a ->
            ResultM.succeed <| Typing.mono name a

        Expr.RecordDestructure _ ->
            Debug.todo ""

        Expr.Spread _ ->
            Debug.todo ""

        Expr.Typeof _ _ ->
            Debug.todo ""

        Expr.VariantDestructure _ _ ->
            Debug.todo ""

        Expr.Wildcard _ ->
            Debug.todo ""



--                                                                            --
-- UNIFICATION -----------------------------------------------------------------
--                                                                            --


{-| A traditional unification algorithm takes a list (technically a set) of types
and calculates a substitution that makes them equal. Our unification algorithm
is extended to also take a list of monomorphic type environments: this is the crux
of the compositional type inference algorithm laid out in both:

    papers/compositional-explanation-of-types.pdf
    papers/compositional-type-checking-for-hindley-milner-type-systems.pdf

-}
unify : List Monoenv -> List Type -> InferM Typing
unify monoenvs ts =
    do next <| \a ->
    do (monoeqs monoenvs) <| \eqs ->
    do (mgu <| eqs ++ List.map (Tuple.pair a) ts) <| \s ->
    do (setSubstitution s) <| \_ ->
    ResultM.succeed <|
        Typing.from
            (List.foldl (Monoenv.merge s) Monoenv.empty monoenvs)
            (Type.substitute s a)


{-| If you consume any of the literal, you will regularly see reference to `mgu`.
This function calculates the _most general_ unifier for a set of type equations.
When we apply the resulting substitution to a type (or typing) we get what is known
as a _principal_ type (or typing). This is key to full-program type inference and
in general a very useful property for our unification algorithm to have.
-}
mgu : List ( Type, Type ) -> InferM Substitution
mgu equations =
    case equations of
        [] ->
            ResultM.succeed Substitution.empty

        ( Var a, t ) :: rest ->
            let
                s =
                    Substitution.singleton a t
            in
            if Var a == t then
                mgu rest

            else if Set.member a (Type.free t) then
                -- This is known as the *occurs check* and it is a check to see if the
                -- type variable `a` appears anywhere in the type `t`. If it does, we
                -- would be dealing with an infinite type on our hands and so we need
                -- to bail.
                --
                -- As an example, say we are trying to unify `t ∪ List t`. Without the
                -- occurs check this would generate a substitution `t ↦ List t` which
                -- if we tried to apply would yield `List (List (List (...)))`!
                ResultM.fail InfiniteType

            else
                --
                List.map (Tuple.mapBoth (Type.substitute s) (Type.substitute s)) rest
                    |> mgu
                    |> ResultM.map (Substitution.compose Type.substitute s)

        -- Instead of duplicating code we just flip the equation and call `mgu`
        -- again.
        ( t, Var a ) :: rest ->
            mgu <| ( Var a, t ) :: rest

        -- For concrete types, there is no unification necessary, but we must
        -- fail if they are not the same.
        ( Con c1, Con c2 ) :: rest ->
            if c1 == c2 then
                mgu rest

            else
                ResultM.fail (IncompatibleTypes (Con c1) (Con c2))

        ( Any, Any ) :: rest ->
            mgu rest

        -- Unifying function types and type applications is the same (in fact,
        -- a function type is really just an application of the `->` type...) and
        -- simply involves unifying each part in sequence.
        ( Fun t1 u1, Fun t2 u2 ) :: rest ->
            mgu <| rest ++ [ ( t1, t2 ), ( u1, u2 ) ]

        ( App t1 u1, App t2 u2 ) :: rest ->
            -- Of course, the number of parameters in our type applications must
            -- be the same length or they cannot possibly be the same type.
            if List.length u1 /= List.length u2 then
                ResultM.fail (IncompatibleTypes (App t1 u1) (App t2 u2))

            else
                mgu <| rest ++ ( t1, t2 ) :: List.map2 Tuple.pair u1 u2

        ( t1, t2 ) :: _ ->
            ResultM.fail (IncompatibleTypes t1 t2)



--                                                                            --
-- UTILS -----------------------------------------------------------------------
--                                                                            --


{-| Instantiate a polymorphic typing by replacing all its free variables with
fresh ones.
-}
inst : Typing -> InferM Typing
inst typing =
    Set.foldl
        (\v inst_ ->
            do inst_ <| \t ->
            do (fresh v) <| \a ->
            ResultM.succeed (Typing.substitute (Substitution.singleton v a) t)
        )
        (ResultM.succeed typing)
        (Typing.free typing)


{-| Lookup a name in the polymorphic context; this is how you would lookup the
type of other functions in a module, for example.
-}
lookup : String -> InferM (Maybe Typing)
lookup name =
    \({ polyenv } as context) ->
        ( context, Ok <| Dict.get name polyenv )


{-| Builtin functions and the types of operators also exist in the polymorphic
environment, but their lookup should never fail. Because of that, we throw an
internal error if the lookup failed so we know something bad has happened.
-}
lookupBuiltin : String -> InferM Typing
lookupBuiltin name =
    do (lookup name) <| \typing ->
    case typing of
        Just t ->
            inst t

        Nothing ->
            ResultM.fail (InternalError <| "Missing typing for builtin: `" ++ name ++ "`")


{-| Generates a new type variable as in `next` but ensures it is fresh by trying
again if the generated variable matches the one passed into `fresh`.
-}
fresh : String -> InferM Type
fresh name =
    do next <| \a ->
    if Var name == a then
        fresh name

    else
        ResultM.succeed a


{-| -}
next : InferM Type
next =
    \({ vars } as context) ->
        case vars of
            var :: rest ->
                ( { context | vars = rest }, Ok (Var var) )

            [] ->
                ( context, Err <| InternalError "Ran out of fresh type variables." )


{-| -}
monoeqs : List Monoenv -> InferM (List ( Type, Type ))
monoeqs monoenvs =
    let
        env =
            List.concatMap Dict.toList monoenvs

        names =
            List.map Tuple.first env |> Set.fromList |> Set.toList

        eq var =
            do next <| \tv ->
            ResultM.succeed ( var, tv )
    in
    do (ResultM.map Dict.fromList (ResultM.mapM eq names)) <| \vars ->
    let
        makeEq ( var, t ) =
            Dict.get var vars |> Maybe.map (\a -> ( a, t ))
    in
    ResultM.succeed <| List.filterMap makeEq env


{-| Extract the current substitution from the inference context.
-}
getSubstitution : InferM Substitution
getSubstitution =
    \({ substitution } as context) ->
        ( context, Ok substitution )


{-| Set the current substitution in the inference context.
-}
setSubstitution : Substitution -> InferM ()
setSubstitution substitution =
    \context ->
        ( { context | substitution = substitution }, Ok () )
