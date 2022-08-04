module Ren.Ast.JavaScript exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Ast.Expr.Lit as Lit
import Ren.Ast.Expr.Op as Op exposing (Op)
import Ren.Ast.Expr.Pat as Pat exposing (Pat)
import Util.List as List
import Util.Math



-- TYPES -----------------------------------------------------------------------


type Statement
    = Block (List Statement)
    | Comment String
    | Const String Expression
    | Expr Expression
    | If Expression Statement (Maybe Statement)
    | Return Expression
    | Throw String


type Expression
    = Access Expression (List String)
    | Add Expression Expression
    | And Expression Expression
    | Array (List Expression)
    | Arrow String Statement
    | Bool Bool
    | Call Expression (List Expression)
    | Div Expression Expression
    | Eq Expression Expression
    | Gt Expression Expression
    | Gte Expression Expression
    | IIFE (Maybe ( String, Expression )) Statement
    | Index Expression Expression
    | Lt Expression Expression
    | Lte Expression Expression
    | Mod Expression Expression
    | Mul Expression Expression
    | Neq Expression Expression
    | Number Float
    | Object (List ( String, Expression ))
    | Or Expression Expression
    | Spread Expression
    | String String
    | Sub Expression Expression
    | Ternary Expression Expression Expression
    | Typeof Expression
    | Undefined
    | Var String



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


fromExpr : Expr -> Statement
fromExpr =
    let
        branchFromCase ( pattern, guard, body ) =
            If (checksFromPattern (Var "$pat") pattern)
                (case guard of
                    Just (Expr g) ->
                        Block <|
                            List.concat
                                [ assignmentsFromPattern (Var "$pat") pattern
                                , [ If g (return body) Nothing ]
                                ]

                    _ ->
                        return <|
                            Block <|
                                List.concat
                                    [ assignmentsFromPattern (Var "$pat") pattern
                                    , [ body ]
                                    ]
                )
                Nothing
    in
    Expr.fold
        { access = \stmt key -> Expr <| Access (asExpression stmt) [ key ]
        , annotated = \stmt _ -> stmt
        , binop = \op lhs rhs -> Expr <| fromOperator op (asExpression lhs) (asExpression rhs)
        , call = \fun args -> Expr <| Call (asExpression fun) (List.map asExpression args)
        , if_ =
            \cond then_ else_ ->
                Expr <| Ternary (asExpression cond) (asExpression then_) (asExpression <| else_)
        , lambda =
            \args body ->
                List.foldr
                    (\pattern stmt ->
                        case pattern of
                            Pat.Var name ->
                                Expr <| Arrow name (return stmt)

                            _ ->
                                [ Throw "Non-exhaustive pattern match", return <| branchFromCase ( pattern, Nothing, stmt ) ]
                                    |> Block
                                    |> Arrow "$pat"
                                    |> Expr
                    )
                    body
                    args
        , let_ =
            \pattern stmt body ->
                case pattern of
                    Pat.Var name ->
                        Block [ Const name (asExpression stmt), body ]

                    _ ->
                        [ Throw "Non-exhaustive pattern match", return <| branchFromCase ( pattern, Nothing, body ) ]
                            |> Block
                            |> IIFE (Just ( "$pat", asExpression stmt ))
                            |> Expr
        , literal =
            \lit ->
                case lit of
                    Lit.Array elements ->
                        Expr <| Array <| List.map asExpression elements

                    Lit.Enum "true" [] ->
                        Expr <| Bool True

                    Lit.Enum "false" [] ->
                        Expr <| Bool False

                    Lit.Enum "undefined" [] ->
                        Expr Undefined

                    Lit.Enum tag args ->
                        Expr <| Array <| String tag :: List.map asExpression args

                    Lit.Number n ->
                        Expr <| Number n

                    Lit.Record fields ->
                        Expr <| Object <| List.map (Tuple.mapSecond asExpression) fields

                    Lit.String s ->
                        Expr <| String s
        , placeholder = Throw "bad placeholder"
        , scoped = \scope name -> Expr <| Var <| String.join "$" scope ++ name
        , where_ =
            \stmt cases ->
                [ Throw "Non-exhaustive pattern match" ]
                    |> (++) (List.map (return << branchFromCase) cases)
                    |> Block
                    |> IIFE (Just ( "$pat", asExpression stmt ))
                    |> Expr
        , var = \var -> Expr <| Var var
        }


fromOperator : Op -> Expression -> Expression -> Expression
fromOperator op lhs rhs =
    case op of
        Op.Add ->
            Add lhs rhs

        Op.And ->
            And lhs rhs

        Op.Concat ->
            Array [ Spread lhs, Spread rhs ]

        Op.Cons ->
            Array [ lhs, Spread rhs ]

        Op.Div ->
            Div lhs rhs

        Op.Eq ->
            Eq lhs rhs

        Op.Gte ->
            Gte lhs rhs

        Op.Gt ->
            Gt lhs rhs

        Op.Lte ->
            Lte lhs rhs

        Op.Lt ->
            Lt lhs rhs

        Op.Mod ->
            Mod lhs rhs

        Op.Mul ->
            Mul lhs rhs

        Op.Neq ->
            Neq lhs rhs

        Op.Or ->
            Or lhs rhs

        Op.Pipe ->
            Call rhs [ lhs ]

        Op.Sub ->
            Sub lhs rhs


checksFromPattern : Expression -> Pat -> Expression
checksFromPattern expr pattern =
    case pattern of
        Pat.Any ->
            Bool True

        Pat.Literal (Lit.Array elements) ->
            elements
                |> List.indexedMap (\i el -> checksFromPattern (Index expr <| Number <| Basics.toFloat i) el)
                -- Patterns like the wildcard `_` are always just true. This is
                -- necessary to handle top-level patterns that match on anything
                -- but once we're inside a container like an array we can just
                -- remove these checks altogether.
                |> List.filter ((/=) (Bool True))
                -- We also want to check the length of the array, if it's not long
                -- enough to satisfy all the other patterns, there's no point trying
                -- any of them!
                |> (::) (Gte (Access expr [ "length" ]) (Number <| Basics.toFloat <| List.length elements))
                -- Finally, we'll do a runtime type check to confirm the value
                -- actually *is* an array. In JavaScript doing `typeof arr` will
                -- (perhaps unintuitively) return `"object"` for arrays, so we
                -- need to call a method on the global `Array` object instead.
                |> List.foldl (\y x -> And x y) (Call (Access (Var "globalThis") [ "Array", "isArray" ]) [ expr ])

        Pat.Literal (Lit.Enum "true" []) ->
            Eq expr <| Bool True

        Pat.Literal (Lit.Enum "false" []) ->
            Eq expr <| Bool False

        Pat.Literal (Lit.Enum "undefined" []) ->
            Eq expr Undefined

        Pat.Literal (Lit.Enum tag args) ->
            -- Variants are represented as arrays at runtime, where the first
            -- element is the tag and the rest are any arguments.
            checksFromPattern expr <| Pat.Literal <| Lit.Array <| (Pat.Literal (Lit.String tag) :: args)

        Pat.Literal (Lit.Number n) ->
            Eq expr <| Number n

        Pat.Literal (Lit.Record fields) ->
            fields
                |> List.map (\( k, v ) -> checksFromPattern (Access expr [ k ]) v)
                -- As with arrays, we can safely remove checks that will always
                -- succeed.
                |> List.filter ((/=) (Bool True))
                |> List.foldl (\y x -> And x y) (Eq (Typeof expr) (String "object"))

        Pat.Literal (Lit.String s) ->
            Eq expr <| String s

        Pat.Type "Array" pat ->
            And
                (Call (Access (Var "Array") [ "isArray" ]) [ expr ])
                (checksFromPattern expr pat)

        Pat.Spread _ ->
            Bool True

        Pat.Type type_ pat ->
            And
                (Or
                    (Eq (Typeof expr) (String type_))
                    (Eq (Access expr [ "constructor", "name" ]) (String type_))
                )
                (checksFromPattern expr pat)

        Pat.Var _ ->
            Bool True


assignmentsFromPattern : Expression -> Pat -> List Statement
assignmentsFromPattern expr pattern =
    case pattern of
        Pat.Any ->
            []

        Pat.Literal (Lit.Array elements) ->
            elements
                |> List.indexedMap (\i el -> assignmentsFromPattern (Index expr <| Number <| Basics.toFloat i) el)
                |> List.concat

        Pat.Literal (Lit.Enum _ args) ->
            assignmentsFromPattern expr <| Pat.Literal <| Lit.Array <| Pat.Any :: args

        Pat.Literal (Lit.Number _) ->
            []

        Pat.Literal (Lit.Record fields) ->
            fields
                |> List.concatMap (\( k, v ) -> assignmentsFromPattern (Access expr [ k ]) v)

        Pat.Literal (Lit.String _) ->
            []

        Pat.Spread name ->
            []

        Pat.Type _ pat ->
            assignmentsFromPattern expr pat

        Pat.Var name ->
            [ Const name expr ]



-- QUERIES ---------------------------------------------------------------------


{-| Used for determining when to wrap subexpressions in parentheses.
As a result, only expressions which emit as multiple terms need to return a value.
Uses precedence numbers from
<https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence>
-}
precedence : Expression -> Int
precedence expr =
    case expr of
        Access _ _ ->
            18

        Call _ _ ->
            18

        Typeof _ ->
            15

        Div _ _ ->
            13

        Mul _ _ ->
            13

        Mod _ _ ->
            13

        Add _ _ ->
            12

        Sub _ _ ->
            12

        Gt _ _ ->
            10

        Gte _ _ ->
            10

        Lt _ _ ->
            10

        Lte _ _ ->
            10

        Eq _ _ ->
            9

        Neq _ _ ->
            9

        And _ _ ->
            5

        Or _ _ ->
            4

        -- The way we check if a current expression should be wrapped in parentheses,
        -- we look at the expression's precedence and compare it to the current
        -- precedence level. If it is lower, it get's wrapped.
        --
        -- Besides operators, other kinds of expression don't really need to deal
        -- with this so we say they have infinite precedence and never wrap.
        _ ->
            Util.Math.infinite


statements : Statement -> List Statement
statements stmt =
    case stmt of
        Block stmts ->
            stmts

        _ ->
            [ stmt ]



-- MANIPULATIONS ---------------------------------------------------------------


flatten : Statement -> Statement
flatten stmt =
    let
        go s =
            case s of
                Block stmts ->
                    List.concatMap go stmts

                _ ->
                    [ s ]
    in
    case stmt of
        Block stmts ->
            Block <| List.concatMap go stmts

        _ ->
            stmt


return : Statement -> Statement
return stmt =
    case stmt of
        Block stmts ->
            Block <| returnLast stmts

        Comment str ->
            Comment str

        Const _ expr ->
            Return expr

        Expr expr ->
            Return expr

        If cond then_ else_ ->
            If cond (return then_) (Maybe.map return else_)

        Return expr ->
            Return expr

        Throw error ->
            Throw error


returnLast : List Statement -> List Statement
returnLast stmts =
    case List.reverse stmts of
        stmt :: rest ->
            List.reverse <| return stmt :: rest

        [] ->
            []



-- CONVERSIONS -----------------------------------------------------------------


asExpression : Statement -> Expression
asExpression stmt =
    case stmt of
        Expr expr ->
            expr

        _ ->
            IIFE Nothing stmt



-- UTILS -----------------------------------------------------------------------
