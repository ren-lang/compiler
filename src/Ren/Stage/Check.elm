module Ren.Stage.Check exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Ast.Decl as Decl exposing (Decl)
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Ast.Expr.Lit as Lit exposing (Lit)
import Ren.Ast.Expr.Op as Op exposing (Op)
import Ren.Ast.Expr.Pat as Pat exposing (Pat)
import Ren.Ast.Type as Type exposing (Type)
import Ren.Control.Eval as Eval exposing (Eval, do, succeed, throw)
import Ren.Control.Eval.Context as Context
import Ren.Data.Monoenv as Monoenv exposing (Monoenv)
import Ren.Data.Polyenv as Polyenv exposing (Polyenv)
import Ren.Data.Subst as Subst exposing (Subst)
import Ren.Data.Typing as Typing exposing (Typing)
import Set
import Util.Dict as Dict
import Util.List as List
import Util.Maybe as Maybe



--


decl : Polyenv -> Dict String Type -> Decl -> Result String Decl
decl env types declaration =
    case declaration of
        Decl.Let meta pub name expression ->
            if meta.tipe == Type.Any then
                Ok declaration

            else if meta.tipe == Type.Hole then
                init env types
                    |> inferDecl meta.tipe (Expr.desugar expression)
                    |> Tuple.second
                    |> Result.map (\typing -> Decl.Let { meta | tipe = Type.simplify <| Typing.type_ typing, inferred = True } pub name expression)

            else
                init env types
                    |> inferDecl meta.tipe (Expr.desugar expression)
                    |> Tuple.second
                    |> Result.map (\typing -> Decl.Let { meta | tipe = Typing.type_ typing } pub name expression)

        Decl.Ext _ _ _ _ ->
            Ok declaration


expr : Polyenv -> Dict String Type -> Expr -> Result String ( Expr, Typing )
expr env types expression =
    init env types
        |> inferExpr (Expr.desugar expression)
        |> Tuple.second
        |> Result.map (Tuple.pair expression)



-- TYPES -----------------------------------------------------------------------


type alias Infer a =
    Eval Context String a


type alias Context =
    { count : Int
    , polyenv : Polyenv
    , subst : Subst Type
    , types : Dict String Type
    }



-- CONSTRUCTORS ----------------------------------------------------------------


init : Polyenv -> Dict String Type -> Context
init polyenv types =
    let
        globalPolyenv =
            [ ( "$add", Type.fun [ Type.num, Type.num ] Type.num )
            , ( "$and", Type.fun [ Type.bool, Type.bool ] Type.bool )
            , ( "$concat", Type.fun [ Type.arr (Type.var "a"), Type.arr (Type.var "a") ] (Type.arr (Type.var "a")) )
            , ( "$cons", Type.fun [ Type.var "a", Type.arr (Type.var "a") ] (Type.arr (Type.var "a")) )
            , ( "$div", Type.fun [ Type.num, Type.num ] Type.num )
            , ( "$eq", Type.fun [ Type.var "a", Type.var "a" ] Type.bool )
            , ( "$gte", Type.fun [ Type.num, Type.num ] Type.bool )
            , ( "$gt", Type.fun [ Type.num, Type.num ] Type.bool )
            , ( "$if", Type.fun [ Type.bool, Type.var "a", Type.var "a" ] (Type.var "a") )
            , ( "$lte", Type.fun [ Type.num, Type.num ] Type.bool )
            , ( "$lt", Type.fun [ Type.num, Type.num ] Type.bool )
            , ( "$mod", Type.fun [ Type.num, Type.num ] Type.num )
            , ( "$mul", Type.fun [ Type.num, Type.num ] Type.num )
            , ( "$neq", Type.fun [ Type.var "a", Type.var "a" ] Type.bool )
            , ( "$or", Type.fun [ Type.bool, Type.bool ] Type.bool )
            , ( "$pipe", Type.fun [ Type.var "a", Type.fun [ Type.var "a" ] (Type.var "b") ] (Type.var "b") )
            , ( "$sub", Type.fun [ Type.num, Type.num ] Type.num )

            -- DEBUGGING THINGS
            -- #just a | #nothing -> a
            , ( "unwrap", Type.fun [ Type.sum [ ( "just", [ Type.var "a" ] ), ( "nothing", [] ) ] ] (Type.var "a") )
            , ( "bad_sum", Type.sum [ ( "just", [ Type.var "a" ] ), ( "nothing", [] ), ( "possibly", [] ) ] )
            , ( "foo", Type.fun [ Type.rec [ ( "foo", Type.var "a" ) ] ] (Type.rec [ ( "foo", Type.var "a" ) ]) )
            , ( "bar", Type.fun [ Type.rec [ ( "bar", Type.var "a" ) ] ] (Type.rec [ ( "bar", Type.var "a" ) ]) )
            , ( "bad_rec", Type.rec [] )
            ]

        globalTypes =
            [ ( "Array", Type.arr (Type.var "a") )
            , ( "Boolean", Type.bool )
            , ( "Number", Type.num )
            , ( "String", Type.str )
            , ( "Undefined", Type.undefined )
            ]
    in
    { count = 0
    , polyenv = List.foldl (\( v, typing ) env -> Polyenv.insert v (Typing.poly typing) env) polyenv globalPolyenv
    , subst = Subst.empty
    , types = List.foldl (\( name, t ) env -> Dict.insert name t env) types globalTypes
    }



-- INFERENCE -------------------------------------------------------------------


inferDecl : Type -> Expr -> Infer Typing
inferDecl expected expression =
    do (inferExpr expression) <| \( exprEnv, exprT ) ->
    do (instantiate <| Typing.poly expected) <| \( expectedEnv, expectedT ) ->
    unify [ exprEnv, expectedEnv ] [ exprT, expectedT ]


inferExpr : Expr -> Infer Typing
inferExpr =
    Expr.fold
        { access = access
        , annotated = annotated
        , binop = binop
        , call = call
        , if_ = if_
        , lambda = lambda
        , let_ = let_
        , literal = literal
        , placeholder = Eval.map Typing.poly next
        , scoped = scoped
        , where_ = where_
        , var = var
        }


access : Infer Typing -> String -> Infer Typing
access inferExpr_ key =
    call (instantiate <| Typing.poly <| Type.access key) [ inferExpr_ ]


annotated : Infer Typing -> Type -> Infer Typing
annotated inferExpr_ expected =
    do inferExpr_ <| \a ->
    do (instantiate <| Typing.poly expected) <| \b ->
    unifyTypings [ a, b ]


binop : Op -> Infer Typing -> Infer Typing -> Infer Typing
binop op inferLhs inferRhs =
    call (lookupBuiltin <| "$" ++ Op.name op) [ inferLhs, inferRhs ]


call : Infer Typing -> List (Infer Typing) -> Infer Typing
call inferFun inferArgs =
    let
        go inferA inferF =
            do inferF <| \( funEnv, funT ) ->
            do inferA <| \( argEnv, argT ) ->
            do next <| \a ->
            case funT of
                Type.Fun expectedArgType expectedReturnType ->
                    -- These coercions are necessary to "lift" up row types. We're
                    -- basically hacking in row subtyping with this, it works and
                    -- it let's do what we want, specifically:
                    --
                    --   * Pass records with more fields than required into functions
                    --   * Pass enums with fewer fields than required into functions
                    --
                    do (coerce expectedArgType argT) <| \x ->
                    unifyCall [ funEnv, argEnv ] [ Type.Fun x expectedReturnType, Type.Fun argT a ]

                _ ->
                    unifyCall [ funEnv, argEnv ] [ funT, Type.Fun argT a ]

        unifyCall envs ts =
            do (unify envs ts) <| \( env, t ) ->
            case t of
                Type.Fun _ r ->
                    do Context.get <| \{ subst } ->
                    succeed <| Typing.substitute subst ( env, r )

                _ ->
                    do next <| \a ->
                    do Context.get <| \{ subst } ->
                    succeed <| Typing.substitute subst ( env, a )
    in
    List.foldl go inferFun inferArgs


if_ : Infer Typing -> Infer Typing -> Infer Typing -> Infer Typing
if_ inferCond inferThen inferElse =
    call (lookupBuiltin "$if") [ inferCond, inferThen, inferElse ]


lambda : List Pat -> Infer Typing -> Infer Typing
lambda patterns inferBody =
    case patterns of
        [] ->
            inferBody

        pattern :: rest ->
            do (pat pattern) <| \( patEnv, patT ) ->
            do (lambda rest inferBody) <| \( bodyEnv, bodyT ) ->
            do next <| \a ->
            do next <| \b ->
            do (unify [ patEnv, bodyEnv ] [ Type.Fun patT a, Type.Fun b bodyT ]) <| \( env, t ) ->
            succeed <| Typing.from (Set.foldl Monoenv.remove env <| Pat.bindings pattern) t


let_ : Pat -> Infer Typing -> Infer Typing -> Infer Typing
let_ pattern inferExpr_ inferBody =
    where_ inferExpr_ [ ( pattern, Nothing, inferBody ) ]


literal : Lit (Infer Typing) -> Infer Typing
literal lit =
    case lit of
        Lit.Array inferElements ->
            Eval.sequence inferElements
                |> Eval.andThen unifyTypings
                |> Eval.map (\( env, t ) -> ( env, Type.arr t ))

        Lit.Enum tag inferArgs ->
            do (Eval.sequence inferArgs) <| \args ->
            let
                ( envs, tN ) =
                    List.unzip args
            in
            do (unifyEnvs envs) <| \env ->
            do Context.get <| \{ subst } ->
            succeed <| Typing.from env <| Type.sum [ ( tag, List.map (Type.substitute subst) tN ) ]

        Lit.Number _ ->
            succeed <| Typing.poly Type.num

        Lit.Record inferKeysAndFields ->
            let
                ( keys, inferFields ) =
                    List.unzip inferKeysAndFields
            in
            do (Eval.sequence inferFields) <| \fields ->
            let
                ( envs, tN ) =
                    List.unzip fields
            in
            do (unifyEnvs envs) <| \env ->
            do Context.get <| \{ subst } ->
            succeed <| Typing.from env <| Type.rec <| List.map2 (\k t -> ( k, Type.substitute subst t )) keys tN

        Lit.String _ ->
            succeed <| Typing.poly Type.str


scoped : List String -> String -> Infer Typing
scoped scope name =
    lookupPolyenv <| String.join "$" scope ++ "$" ++ name


where_ : Infer Typing -> List ( Pat, Maybe (Infer Typing), Infer Typing ) -> Infer Typing
where_ inferExpr_ inferCases =
    let
        case_ ( pattern, maybeInferGuard, inferBody ) =
            do (Maybe.withDefault (succeed <| Typing.poly Type.bool) maybeInferGuard) <| \( guardEnv, guardT ) ->
            do (unify [ guardEnv ] [ guardT, Type.bool ]) <| \_ ->
            lambda [ pattern ] inferBody
    in
    do (Eval.traverse case_ inferCases) <| \cases ->
    do (unifyTypings cases) <| \( caseEnv, caseT ) ->
    do inferExpr_ <| \( exprEnv, exprT ) ->
    do next <| \a ->
    do (unify [ exprEnv, caseEnv ] [ Type.Fun exprT a, caseT ]) <| \( env, t ) ->
    case t of
        Type.Fun _ r ->
            succeed <| Typing.from env r

        _ ->
            throw <| "[Where] Expected function type, got " ++ Type.toString t


var : String -> Infer Typing
var name =
    lookupPolyenv name |> Eval.andCatch (\_ -> Eval.map (Typing.mono name) next)



--


pat : Pat -> Infer Typing
pat pattern =
    case pattern of
        Pat.Any ->
            do next <| \a ->
            succeed <| Typing.poly a

        Pat.Literal (Lit.Array elements) ->
            Eval.traverse pat elements
                |> Eval.andThen unifyTypings
                |> Eval.map (Typing.updateType Type.arr)

        Pat.Literal (Lit.Enum tag args) ->
            do (Eval.traverse pat args) <| \args_ ->
            let
                ( envs, tN ) =
                    List.unzip args_
            in
            do (unifyEnvs envs) <| \env ->
            do Context.get <| \{ subst } ->
            succeed <| Typing.from env <| Type.sum [ ( tag, List.map (Type.substitute subst) tN ) ]

        Pat.Literal (Lit.Number _) ->
            succeed <| Typing.poly Type.num

        Pat.Literal (Lit.Record fields) ->
            let
                ( keys, pats ) =
                    List.unzip fields
            in
            do (Eval.traverse pat pats) <| \typings ->
            let
                ( envs, tN ) =
                    List.unzip typings
            in
            do (unifyEnvs envs) <| \env ->
            do Context.get <| \{ subst } ->
            succeed <| Typing.from env <| Type.rec <| List.map2 (\k t -> ( k, Type.substitute subst t )) keys tN

        Pat.Literal (Lit.String _) ->
            succeed <| Typing.poly Type.str

        Pat.Spread name ->
            do next <| \a ->
            succeed <| Typing.mono name <| Type.arr a

        Pat.Type _ p ->
            do (pat p) <| \typing ->
            succeed <| Typing.from (Typing.env typing) Type.Any

        Pat.Var name ->
            do next <| \a ->
            succeed <| Typing.mono name a



-- UNIFICATION -----------------------------------------------------------------


unify : List Monoenv -> List Type -> Infer Typing
unify envs tN =
    do next <| \a ->
    do (monoeqs envs) <| \eqs ->
    do (mgu <| eqs ++ List.map (Tuple.pair a) tN) <| \s ->
    do (Context.update (\ctx -> { ctx | subst = s })) <| \_ ->
    succeed <| Typing.from (List.foldl (Monoenv.merge s) Monoenv.empty envs) (Type.substitute s a)


unifyTypings : List Typing -> Infer Typing
unifyTypings typings =
    List.unzip typings |> (\( envs, tN ) -> unify envs tN)


unifyEnvs : List Monoenv -> Infer Monoenv
unifyEnvs envs =
    unify envs [] |> Eval.map Tuple.first


monoeqs : List Monoenv -> Infer (List ( Type, Type ))
monoeqs envs =
    let
        env =
            List.concatMap Monoenv.toList envs

        names =
            env
                |> List.map Tuple.first
                |> List.uniques

        freshen v =
            Eval.map (Tuple.pair v) next

        eq vars ( name, t ) =
            Dict.get name vars
                |> Maybe.map (\a -> ( a, t ))
    in
    Eval.traverse freshen names
        |> Eval.map Dict.fromList
        |> Eval.map (\vars -> List.filterMap (eq vars) env)


coerce : Type -> Type -> Infer Type
coerce gotT expectedT =
    let
        containsField row l =
            Dict.member l row

        freshenField tN =
            Eval.sequence <| List.repeat (List.length tN) next

        freshenRow l tN row =
            Eval.map2 (\v d -> Dict.update l (Maybe.or (Just v)) d) (freshenField tN) row
    in
    case ( gotT, expectedT ) of
        ( _, Type.Any ) ->
            succeed Type.Any

        ( Type.Any, _ ) ->
            throw "type mismatch"

        ( Type.App t1 u1, Type.App t2 u2 ) ->
            do (coerce t1 t2) <| \t ->
            do (Eval.sequence <| List.map2 coerce u1 u2) <| \u ->
            succeed <| Type.App t u

        ( Type.Fun t1 r1, Type.Fun t2 r2 ) ->
            do (coerce t1 t2) <| \t ->
            do (coerce r1 r2) <| \r ->
            succeed <| Type.Fun t r

        -- A record with more rows can always be used in place of a record with
        -- fewer rows. If we have a function:
        --
        --   foo : { name : String } -> String
        --
        -- we should be able to call it with:
        --
        --   foo : { name : "Hayleigh", is_cool : #true }
        --
        -- so this essentially returns a new record with the extra fields filled
        -- in with fresh type variables.
        ( Type.Rec gotRow, Type.Rec expectedRow ) ->
            -- First check the rec type is smaller (aka a possible subtype) of
            -- the expected rec type:
            --
            --   Dict.size gotRow < Dict.size expectedRow
            --
            -- Then check that all the rows in the rec type are contained in the
            -- expected sum type:
            --
            --   List.all (containsField expectedRow) (Dict.keys gotRow)
            --
            if Dict.isEmpty gotRow && Basics.not (Dict.isEmpty expectedRow) then
                succeed gotT

            else if Dict.size gotRow <= Dict.size expectedRow && List.all (containsField expectedRow) (Dict.keys gotRow) then
                Dict.foldl freshenRow (succeed gotRow) expectedRow
                    |> Eval.map Type.Rec

            else
                succeed gotT

        -- A sum with fewer rows can always be used in place of a record with more
        -- rows. If we have a function:
        --
        --   foo : #just a | #nothing -> a
        --
        -- we should be able to call it with:
        --
        --   foo (#just 10)
        --
        ( Type.Sum gotRow, Type.Sum expectedRow ) ->
            -- First check the sum type is smaller (aka a possible subtype) of
            -- the expected sum type:
            --
            --   Dict.size gotRow < Dict.size expectedRow
            --
            -- Then check that all the rows in the sum type are contained in the
            -- expected sum type:
            --
            --   List.all (containsField expectedRow) (Dict.keys gotRow)
            --
            if Dict.size gotRow <= Dict.size expectedRow && List.all (containsField expectedRow) (Dict.keys gotRow) then
                succeed gotT

            else
                succeed expectedT

        _ ->
            succeed gotT


mgu : List ( Type, Type ) -> Infer (Subst Type)
mgu eqs =
    case eqs of
        [] ->
            succeed Subst.empty

        ( Type.Any, Type.Any ) :: rest ->
            mgu rest

        ( Type.App t1 u1, Type.App t2 u2 ) :: rest ->
            if List.length u1 == List.length u2 then
                mgu <| ( t1, t2 ) :: List.map2 Tuple.pair u1 u2 ++ rest

            else
                throw "type mismatch"

        ( Type.Con c1, Type.Con c2 ) :: rest ->
            do Eval.context <| \{ types } ->
            if c1 == c2 && Dict.member c1 types && Dict.member c2 types then
                mgu rest

            else
                throw "type mismatch"

        ( Type.Fun t1 u1, Type.Fun t2 u2 ) :: rest ->
            mgu <| [ ( t1, t2 ), ( u1, u2 ) ] ++ rest

        ( Type.Hole, _ ) :: rest ->
            mgu rest

        ( _, Type.Hole ) :: rest ->
            mgu rest

        ( Type.Rec r1, Type.Rec r2 ) :: rest ->
            if r1 == r2 then
                mgu rest

            else if Dict.size r1 /= Dict.size r2 then
                throw "type mismatch"

            else
                List.map2 Tuple.pair (Dict.toList r1) (Dict.toList r2)
                    |> List.foldr
                        (\( ( l1, t1 ), ( l2, t2 ) ) tN ->
                            if l1 == l2 && List.length t1 == List.length t2 then
                                Eval.map ((++) (List.map2 Tuple.pair t1 t2)) tN

                            else
                                throw "type mismatch"
                        )
                        (succeed rest)
                    |> Eval.andThen mgu

        ( Type.Sum r1, Type.Sum r2 ) :: rest ->
            -- Unification is the same for both records and sum types, so instead
            -- of typing it out twice we'll just pretend we have a record.
            mgu <| ( Type.Rec r1, Type.Rec r2 ) :: rest

        ( Type.Var v, t ) :: rest ->
            let
                s =
                    Subst.singleton v t
            in
            if Type.Var v == t then
                mgu rest

            else if Set.member v (Type.free t) then
                throw "occurs check failed"

            else
                List.map (Tuple.mapBoth (Type.substitute s) (Type.substitute s)) rest
                    |> mgu
                    |> Eval.map (Subst.compose Type.substitute s)

        ( t, Type.Var v ) :: rest ->
            mgu <| ( Type.Var v, t ) :: rest

        _ ->
            throw "type mismatch"



-- CONTEXT ---------------------------------------------------------------------


fresh : String -> Infer Type
fresh name =
    Eval.do next <| \a ->
    if a == Type.var name then
        fresh name

    else
        succeed a


next : Infer Type
next =
    do Context.get <| \{ count } ->
    do (Context.update (\ctx -> { ctx | count = count + 1 })) <| \_ ->
    succeed <| Type.var <| Type.fresh count


instantiate : Typing -> Infer Typing
instantiate typing =
    Set.foldl
        (\v inst ->
            do inst <| \t ->
            do (fresh v) <| \a ->
            succeed <| Typing.substitute (Subst.singleton v a) t
        )
        (succeed typing)
        (Typing.free typing)


lookupPolyenv : String -> Infer Typing
lookupPolyenv v =
    do Context.get <| \{ polyenv } ->
    case Polyenv.lookup v polyenv of
        Just typing ->
            instantiate typing

        Nothing ->
            throw "variable not in scope"


extendPolyenv : String -> Typing -> Infer ()
extendPolyenv v typing =
    Context.update
        (\({ polyenv } as ctx) ->
            { ctx | polyenv = Polyenv.insert v typing polyenv }
        )


lookupBuiltin : String -> Infer Typing
lookupBuiltin v =
    lookupPolyenv v |> Eval.andCatch (\_ -> throw "could not find builtin")
