module Ren.Queries.Check exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Ren.Ast.Decl as Decl
import Ren.Ast.Decl.Ext as Ext
import Ren.Ast.Decl.Imp as Imp
import Ren.Ast.Decl.Let as Let
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Ast.Expr.Lit as Lit exposing (Lit)
import Ren.Ast.Expr.Op as Op exposing (Op)
import Ren.Ast.Expr.Pat as Pat exposing (Pat)
import Ren.Ast.Mod as Mod exposing (Mod)
import Ren.Ast.Type as Type exposing (Type)
import Ren.Control.Query as Query exposing (Query)
import Ren.Control.Query.Env as Env
import Ren.Data.Monoenv as Monoenv exposing (Monoenv)
import Ren.Data.Subst as Subst exposing (Subst)
import Ren.Data.Typing as Typing exposing (Typing)
import Ren.Queries.Parse as Parse
import Set
import Util.Dict as Dict
import Util.List as List
import Util.Maybe as Maybe


decl : String -> String -> Query (Env r) Typing
decl path name =
    Query.do (Query.succeed ()) <| \_ ->
    Query.do (Parse.file path) <| \this ->
    case Mod.lookup name this of
        Just (Decl.Let l) ->
            case (Let.meta l).tipe of
                Type.Any ->
                    Query.succeed <| Typing.poly Type.Any

                --
                Type.Hole ->
                    case Let.annotation l of
                        Just t ->
                            Query.do (init path this) <| \_ ->
                            Query.do (inferExpr <| Let.body l) <| \got ->
                            Query.do (instantiate <| Typing.poly t) <| \expected ->
                            Query.do (unifyTypings [ expected, got ]) <| \typing ->
                            Query.do
                                (Env.update
                                    (\env ->
                                        { env
                                            | this = Mod.updateBinding name (Let.setType <| Typing.type_ typing) env.this
                                            , modules = Dict.update env.path (Maybe.map (Mod.updateBinding name (Let.setType <| Typing.type_ typing))) env.modules
                                        }
                                    )
                                )
                            <| \_ ->
                            Query.succeed <| typing

                        Nothing ->
                            Query.do (init path <| Mod.updateBinding name (Let.setType Type.Any) this) <| \_ ->
                            Query.succeed <| Typing.poly Type.Any

                t ->
                    Query.succeed <| Typing.poly t

        Just (Decl.Ext e) ->
            Query.do (init path this) <| \_ ->
            Query.do (instantiate <| Typing.poly <| (Ext.meta e).tipe) <| \typing ->
            Query.succeed typing

        _ ->
            Query.fail <| "Cannot find declaration '" ++ name ++ "' in module."



-- TYPES -----------------------------------------------------------------------


type alias Env r =
    { r
        | count : Int
        , subst : Subst Type
        , path : String
        , this : Mod
        , modules : Dict String Mod
    }


type alias LocalEnv =
    { count : Int
    , subst : Subst Type
    , path : String
    , this : Mod
    , modules : Dict String Mod
    }



-- CONSTRUCTORS ----------------------------------------------------------------


init : String -> Mod -> Query (Env r) ()
init path this =
    let
        prim =
            List.map Decl.Ext <|
                [ Ext.new True "add" (Just <| Type.fun [ Type.num, Type.num ] Type.num) ""
                , Ext.new True "and" (Just <| Type.fun [ Type.bool, Type.bool ] Type.bool) ""
                , Ext.new True "concat" (Just <| Type.fun [ Type.arr (Type.var "a"), Type.arr (Type.var "a") ] (Type.arr (Type.var "a"))) ""
                , Ext.new True "cons" (Just <| Type.fun [ Type.var "a", Type.arr (Type.var "a") ] (Type.arr (Type.var "a"))) ""
                , Ext.new True "div" (Just <| Type.fun [ Type.num, Type.num ] Type.num) ""
                , Ext.new True "eq" (Just <| Type.fun [ Type.var "a", Type.var "a" ] Type.bool) ""
                , Ext.new True "gte" (Just <| Type.fun [ Type.num, Type.num ] Type.bool) ""
                , Ext.new True "gt" (Just <| Type.fun [ Type.num, Type.num ] Type.bool) ""
                , Ext.new True "if" (Just <| Type.fun [ Type.bool, Type.var "a", Type.var "a" ] (Type.var "a")) ""
                , Ext.new True "lte" (Just <| Type.fun [ Type.num, Type.num ] Type.bool) ""
                , Ext.new True "lt" (Just <| Type.fun [ Type.num, Type.num ] Type.bool) ""
                , Ext.new True "mod" (Just <| Type.fun [ Type.num, Type.num ] Type.num) ""
                , Ext.new True "mul" (Just <| Type.fun [ Type.num, Type.num ] Type.num) ""
                , Ext.new True "neq" (Just <| Type.fun [ Type.var "a", Type.var "a" ] Type.bool) ""
                , Ext.new True "or" (Just <| Type.fun [ Type.bool, Type.bool ] Type.bool) ""
                , Ext.new True "pipe" (Just <| Type.fun [ Type.var "a", Type.fun [ Type.var "a" ] (Type.var "b") ] (Type.var "b")) ""
                , Ext.new True "sub" (Just <| Type.fun [ Type.num, Type.num ] Type.num) ""
                ]
    in
    Env.update
        (\env ->
            { env
                | count = 0
                , subst = Subst.empty
                , path = path
                , this = this
                , modules =
                    env.modules
                        |> Dict.insert "prim" (Mod.new "prim" prim)
                        |> Dict.insert path this
            }
        )



-- , types = List.foldl (\( name, t ) env -> Dict.insert name t env) types globalTypes
-- INFERENCE -------------------------------------------------------------------


inferDecl : Type -> Expr -> Query (Env r) Typing
inferDecl expected expression =
    Query.do (inferExpr expression) <| \( exprEnv, exprT ) ->
    Query.do (instantiate <| Typing.poly expected) <| \( expectedEnv, expectedT ) ->
    unify [ exprEnv, expectedEnv ] [ exprT, expectedT ]


inferExpr : Expr -> Query (Env r) Typing
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
        , placeholder = Query.map Typing.poly next
        , scoped = lookup
        , where_ = where_
        , var = var
        }


access : Query (Env r) Typing -> String -> Query (Env r) Typing
access inferExpr_ key =
    call (instantiate <| Typing.poly <| Type.access key) [ inferExpr_ ]


annotated : Query (Env r) Typing -> Type -> Query (Env r) Typing
annotated inferExpr_ expected =
    Query.do inferExpr_ <| \a ->
    Query.do (instantiate <| Typing.poly expected) <| \b ->
    unifyTypings [ a, b ]


binop : Op -> Query (Env r) Typing -> Query (Env r) Typing -> Query (Env r) Typing
binop op inferLhs inferRhs =
    call (lookupPrim <| Op.name op) [ inferLhs, inferRhs ]


call : Query (Env r) Typing -> List (Query (Env r) Typing) -> Query (Env r) Typing
call inferFun inferArgs =
    let
        go inferA inferF =
            Query.do inferF <| \( funEnv, funT ) ->
            Query.do inferA <| \( argEnv, argT ) ->
            Query.do next <| \a ->
            case funT of
                Type.Fun expectedArgType expectedReturnType ->
                    -- These coercions are necessary to "lift" up row types. We're
                    -- basically hacking in row subtyping with this, it works and
                    -- it let's Query.do what we want, specifically:
                    --
                    --   * Pass records with more fields than required into functions
                    --   * Pass enums with fewer fields than required into functions
                    --
                    Query.do (coerce expectedArgType argT) <| \x ->
                    unifyCall [ funEnv, argEnv ] [ Type.Fun x expectedReturnType, Type.Fun argT a ]

                _ ->
                    unifyCall [ funEnv, argEnv ] [ funT, Type.Fun argT a ]

        unifyCall envs ts =
            Query.do (unify envs ts) <| \( env, t ) ->
            case t of
                Type.Fun _ r ->
                    Query.do Env.get <| \{ subst } ->
                    Query.succeed <| Typing.substitute subst ( env, r )

                _ ->
                    Query.do next <| \a ->
                    Query.do Env.get <| \{ subst } ->
                    Query.succeed <| Typing.substitute subst ( env, a )
    in
    List.foldl go inferFun inferArgs


if_ : Query (Env r) Typing -> Query (Env r) Typing -> Query (Env r) Typing -> Query (Env r) Typing
if_ inferCond inferThen inferElse =
    call (lookupPrim "if") [ inferCond, inferThen, inferElse ]


lambda : List Pat -> Query (Env r) Typing -> Query (Env r) Typing
lambda patterns inferBody =
    case patterns of
        [] ->
            inferBody

        pattern :: rest ->
            Query.do (pat pattern) <| \( patEnv, patT ) ->
            Query.do (lambda rest inferBody) <| \( bodyEnv, bodyT ) ->
            Query.do next <| \a ->
            Query.do next <| \b ->
            Query.do (unify [ patEnv, bodyEnv ] [ Type.Fun patT a, Type.Fun b bodyT ]) <| \( env, t ) ->
            Query.succeed <| Typing.from (Set.foldl Monoenv.remove env <| Pat.bindings pattern) t


let_ : Pat -> Query (Env r) Typing -> Query (Env r) Typing -> Query (Env r) Typing
let_ pattern inferExpr_ inferBody =
    where_ inferExpr_ [ ( pattern, Nothing, inferBody ) ]


literal : Lit (Query (Env r) Typing) -> Query (Env r) Typing
literal lit =
    case lit of
        Lit.Array inferElements ->
            Query.sequence inferElements
                |> Query.andThen unifyTypings
                |> Query.map (\( env, t ) -> ( env, Type.arr t ))

        Lit.Enum tag inferArgs ->
            Query.do (Query.sequence inferArgs) <| \args ->
            let
                ( envs, tN ) =
                    List.unzip args
            in
            Query.do (unifyEnvs envs) <| \env ->
            Query.do Env.get <| \{ subst } ->
            Query.succeed <| Typing.from env <| Type.sum [ ( tag, List.map (Type.substitute subst) tN ) ]

        Lit.Number _ ->
            Query.succeed <| Typing.poly Type.num

        Lit.Record inferKeysAndFields ->
            let
                ( keys, inferFields ) =
                    List.unzip inferKeysAndFields
            in
            Query.do (Query.sequence inferFields) <| \fields ->
            let
                ( envs, tN ) =
                    List.unzip fields
            in
            Query.do (unifyEnvs envs) <| \env ->
            Query.do Env.get <| \{ subst } ->
            Query.succeed <| Typing.from env <| Type.rec <| List.map2 (\k t -> ( k, Type.substitute subst t )) keys tN

        Lit.String _ ->
            Query.succeed <| Typing.poly Type.str


where_ : Query (Env r) Typing -> List ( Pat, Maybe (Query (Env r) Typing), Query (Env r) Typing ) -> Query (Env r) Typing
where_ inferExpr_ inferCases =
    let
        case_ ( pattern, maybeInferGuard, inferBody ) =
            Query.do (Maybe.withDefault (Query.succeed <| Typing.poly Type.bool) maybeInferGuard) <| \( guardEnv, guardT ) ->
            Query.do (unify [ guardEnv ] [ guardT, Type.bool ]) <| \_ ->
            lambda [ pattern ] inferBody
    in
    Query.do (Query.traverse case_ inferCases) <| \cases ->
    Query.do (unifyTypings cases) <| \( caseEnv, caseT ) ->
    Query.do inferExpr_ <| \( exprEnv, exprT ) ->
    Query.do next <| \a ->
    Query.do (unify [ exprEnv, caseEnv ] [ Type.Fun exprT a, caseT ]) <| \( env, t ) ->
    case t of
        Type.Fun _ r ->
            Query.succeed <| Typing.from env r

        _ ->
            Query.fail <| "[Where] Expected function type, got " ++ Type.toString t


var : String -> Query (Env r) Typing
var name =
    Query.try (lookup [] name) (\_ -> Query.map (Typing.mono name) next) <| \typing ->
    Query.succeed <| typing



--


pat : Pat -> Query (Env r) Typing
pat pattern =
    case pattern of
        Pat.Any ->
            Query.do next <| \a ->
            Query.succeed <| Typing.poly a

        Pat.Literal (Lit.Array elements) ->
            Query.traverse pat elements
                |> Query.andThen unifyTypings
                |> Query.map (Typing.updateType Type.arr)

        Pat.Literal (Lit.Enum tag args) ->
            Query.do (Query.traverse pat args) <| \args_ ->
            let
                ( envs, tN ) =
                    List.unzip args_
            in
            Query.do (unifyEnvs envs) <| \env ->
            Query.do Env.get <| \{ subst } ->
            Query.succeed <| Typing.from env <| Type.sum [ ( tag, List.map (Type.substitute subst) tN ) ]

        Pat.Literal (Lit.Number _) ->
            Query.succeed <| Typing.poly Type.num

        Pat.Literal (Lit.Record fields) ->
            let
                ( keys, pats ) =
                    List.unzip fields
            in
            Query.do (Query.traverse pat pats) <| \typings ->
            let
                ( envs, tN ) =
                    List.unzip typings
            in
            Query.do (unifyEnvs envs) <| \env ->
            Query.do Env.get <| \{ subst } ->
            Query.succeed <| Typing.from env <| Type.rec <| List.map2 (\k t -> ( k, Type.substitute subst t )) keys tN

        Pat.Literal (Lit.String _) ->
            Query.succeed <| Typing.poly Type.str

        Pat.Spread name ->
            Query.do next <| \a ->
            Query.succeed <| Typing.mono name <| Type.arr a

        Pat.Type _ p ->
            Query.do (pat p) <| \typing ->
            Query.succeed <| Typing.from (Typing.env typing) Type.Any

        Pat.Var name ->
            Query.do next <| \a ->
            Query.succeed <| Typing.mono name a



-- UNIFICATION -----------------------------------------------------------------


unify : List Monoenv -> List Type -> Query (Env r) Typing
unify envs tN =
    Query.do next <| \a ->
    Query.do (monoeqs envs) <| \eqs ->
    Query.do (mgu <| eqs ++ List.map (Tuple.pair a) tN) <| \s ->
    Query.do (Env.update (\ctx -> { ctx | subst = s })) <| \_ ->
    Query.succeed <| Typing.from (List.foldl (Monoenv.merge s) Monoenv.empty envs) (Type.substitute s a)


unifyTypings : List Typing -> Query (Env r) Typing
unifyTypings typings =
    List.unzip typings |> (\( envs, tN ) -> unify envs tN)


unifyEnvs : List Monoenv -> Query (Env r) Monoenv
unifyEnvs envs =
    unify envs [] |> Query.map Tuple.first


monoeqs : List Monoenv -> Query (Env r) (List ( Type, Type ))
monoeqs envs =
    let
        env =
            List.concatMap Monoenv.toList envs

        names =
            env
                |> List.map Tuple.first
                |> List.uniques

        freshen v =
            Query.map (Tuple.pair v) next

        eq vars ( name, t ) =
            Dict.get name vars
                |> Maybe.map (\a -> ( a, t ))
    in
    Query.traverse freshen names
        |> Query.map Dict.fromList
        |> Query.map (\vars -> List.filterMap (eq vars) env)


coerce : Type -> Type -> Query (Env r) Type
coerce gotT expectedT =
    let
        containsField row l =
            Dict.member l row

        freshenField tN =
            Query.sequence <| List.repeat (List.length tN) next

        freshenRow l tN row =
            Query.map2 (\v d -> Dict.update l (Maybe.or (Just v)) d) (freshenField tN) row
    in
    case ( gotT, expectedT ) of
        ( _, Type.Any ) ->
            Query.succeed Type.Any

        ( Type.Any, _ ) ->
            Query.fail <| "type mismatch: can't coerce " ++ Type.toString gotT ++ " to " ++ Type.toString expectedT

        ( Type.App t1 u1, Type.App t2 u2 ) ->
            Query.do (coerce t1 t2) <| \t ->
            Query.do (Query.sequence <| List.map2 coerce u1 u2) <| \u ->
            Query.succeed <| Type.App t u

        ( Type.Fun t1 r1, Type.Fun t2 r2 ) ->
            Query.do (coerce t1 t2) <| \t ->
            Query.do (coerce r1 r2) <| \r ->
            Query.succeed <| Type.Fun t r

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
                Query.succeed gotT

            else if Dict.size gotRow <= Dict.size expectedRow && List.all (containsField expectedRow) (Dict.keys gotRow) then
                Dict.foldl freshenRow (Query.succeed gotRow) expectedRow
                    |> Query.map Type.Rec

            else
                Query.succeed gotT

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
                Query.succeed gotT

            else
                Query.succeed expectedT

        _ ->
            Query.succeed gotT


mgu : List ( Type, Type ) -> Query (Env r) (Subst Type)
mgu eqs =
    case eqs of
        [] ->
            Query.succeed Subst.empty

        ( Type.Any, Type.Any ) :: rest ->
            mgu rest

        ( (Type.App t1 u1) as x, (Type.App t2 u2) as y ) :: rest ->
            if List.length u1 == List.length u2 then
                mgu <| ( t1, t2 ) :: List.map2 Tuple.pair u1 u2 ++ rest

            else
                Query.fail <| "type mismatch: " ++ Type.toString t1 ++ " and " ++ Type.toString t2 ++ " have different arities"

        ( Type.Con c1, Type.Con c2 ) :: rest ->
            -- TODO: We should come up with an actual way to resolve types the
            -- same way we resolve variables. This just hardcodes the `prim` ones.
            if c1 == c2 && (c1 == "Array" || c1 == "Num" || c1 == "Str") then
                mgu rest

            else
                Query.fail <| "type mismatch: " ++ c1 ++ " and " ++ c2 ++ " are not the same type"

        ( Type.Fun t1 u1, Type.Fun t2 u2 ) :: rest ->
            mgu <| [ ( t1, t2 ), ( u1, u2 ) ] ++ rest

        ( Type.Hole, _ ) :: rest ->
            mgu rest

        ( _, Type.Hole ) :: rest ->
            mgu rest

        ( (Type.Rec r1) as x, (Type.Rec r2) as y ) :: rest ->
            if r1 == r2 then
                mgu rest

            else if Dict.size r1 /= Dict.size r2 then
                Query.fail <| "type mismatch: " ++ Type.toString x ++ " and " ++ Type.toString y ++ " have different arities"

            else
                List.map2 Tuple.pair (Dict.toList r1) (Dict.toList r2)
                    |> List.foldr
                        (\( ( l1, t1 ), ( l2, t2 ) ) tN ->
                            if l1 == l2 && List.length t1 == List.length t2 then
                                Query.map ((++) (List.map2 Tuple.pair t1 t2)) tN

                            else
                                Query.fail <| "type mismatch: " ++ Type.toString x ++ " and " ++ Type.toString y ++ " have different arities"
                        )
                        (Query.succeed rest)
                    |> Query.andThen mgu

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
                Query.fail "occurs check failed"

            else
                List.map (Tuple.mapBoth (Type.substitute s) (Type.substitute s)) rest
                    |> mgu
                    |> Query.map (Subst.compose Type.substitute s)

        ( t, Type.Var v ) :: rest ->
            mgu <| ( Type.Var v, t ) :: rest

        ( x, y ) :: _ ->
            Query.fail <| "type mismatch: " ++ Type.toString x ++ " and " ++ Type.toString y ++ " are not the compatible types"



-- CONTEXT ---------------------------------------------------------------------


fresh : String -> Query (Env r) Type
fresh name =
    Query.do next <| \a ->
    if a == Type.var name then
        fresh name

    else
        Query.succeed a


next : Query (Env r) Type
next =
    Query.do Env.get <| \{ count } ->
    Query.do (Env.update (\ctx -> { ctx | count = count + 1 })) <| \_ ->
    Query.succeed <| Type.var <| Type.fresh count


instantiate : Typing -> Query (Env r) Typing
instantiate typing =
    Set.foldl
        (\v inst ->
            Query.do inst <| \t ->
            Query.do (fresh v) <| \a ->
            Query.succeed <| Typing.substitute (Subst.singleton v a) t
        )
        (Query.succeed typing)
        (Typing.free typing)


lookup : List String -> String -> Query (Env r) Typing
lookup scope v =
    Query.do Env.get <| \env ->
    case scope of
        [] ->
            decl env.path v

        _ ->
            Dict.values env.this.imports
                |> List.find (Imp.qualified >> (==) scope)
                |> Maybe.map
                    (\imp ->
                        Query.do (decl (Imp.path imp) v) <| \typing ->
                        Query.do Env.get <| \{ modules } ->
                        Query.do (Env.set { env | this = env.this, modules = modules }) <| \_ ->
                        instantiate typing
                    )
                |> Maybe.withDefault (Query.fail "variable not in scope")


lookupPrim : String -> Query (Env r) Typing
lookupPrim v =
    Query.do Env.get <| \env ->
    Query.do (decl "prim" v) <| \typing ->
    Query.do Env.get <| \{ modules } ->
    Query.do (Env.set { env | this = env.this, modules = modules }) <| \_ ->
    instantiate typing
