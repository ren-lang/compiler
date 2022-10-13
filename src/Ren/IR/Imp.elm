module Ren.IR.Imp exposing (..)

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
    | Break
    | Comment String
    | Const String Expression
    | Continue
    | Expr Expression
    | Export Statement
    | FunctionDeclaration String (List String) (List Statement)
    | ForIn String Expression Statement
    | If Expression Statement (Maybe Statement)
    | Return Expression
    | Throw String
    | While Expression Statement


type Expression
    = Access Expression (List String)
    | Array (List Expression)
    | Assign Expression Expression
    | Binop Expression BinaryOperator Expression
    | Call Expression (List Expression)
    | Function (List String) (List Statement)
    | JSFalse
    | IIFE Statement
    | Index Expression Expression
    | Null
    | Number Float
    | Object (List ( String, Expression ))
    | Spread Expression
    | String String
    | Ternary Expression Expression Expression
    | JSTrue
    | Undefined
    | Unop UnaryOperator Expression
    | Var String


type UnaryOperator
    = Neg
    | New
    | Not
    | Pos
    | Typeof


type BinaryOperator
    = Add
    | And
    | Comma
    | Div
    | Eq
    | Gt
    | Gte
    | In
    | Instanceof
    | Lt
    | Lte
    | Mod
    | Mul
    | Neq
    | Or
    | Sub



-- CONSTANTS -------------------------------------------------------------------


prelude : List Statement
prelude =
    [ Comment "This utility gets applied to all functions defined in ren: top-level"
    , Comment "declarations, let bindings, or anonymous functions. It allows us"
    , Comment "to support partial application of functions without explicit"
    , Comment "currying."
    , Comment ""
    , Comment "Any external imports are also automatically wrapped in this utility,"
    , Comment "which means external JavaScript will also support partial application!"
    , Comment ""
    , Comment "Beyond some performance benefits, another reason this utility is"
    , Comment "useful is that it makes it easier to consume ren code in JavaScript."
    , Comment "If ren functions were auto-curried, external JavaScript would need"
    , Comment "to call ren functions like `add(1)(2)` which is not very idiomatic!"

    -- function $function (f, thisArg) {
    --     if (typeof f == "object" || Array.isArray(f)) {
    --         for (const k in f) {
    --             k[f] = $function(k[f], f)
    --         }
    --     }
    --
    --     if (typeof f != "function") {
    --         return f
    --     }
    --
    --     return (...args) => {
    --         if (args.length == f.length) {
    --             return f(...args)
    --         }
    --
    --         if (args.length > f.length) {
    --             return $function(f(...args.slice(0, f.length)), thisArg)(...args.slice(f.length))
    --         }
    --
    --         return $function(f.bind(thisArg, ...args), thisArg)
    --     }
    -- }
    , Export <|
        FunctionDeclaration "$function" [ "f", "thisArg" ] <|
            [ If (Binop (Binop (Unop Typeof (Var "f")) Eq (String "object")) Or (Call (Access (Var "Array") [ "isArray" ]) [ Var "f" ]))
                (ForIn "k" (Var "f") <|
                    Expr <|
                        Assign (Index (Var "k") (Var "f")) (Call (Var "$function") [ Index (Var "k") (Var "f"), Var "f" ])
                )
                Nothing
            , If (Binop (Unop Typeof (Var "f")) Neq (String "function"))
                (Return (Var "f"))
                Nothing
            , Return <|
                Function [ "...args" ] <|
                    [ If (Binop (Access (Var "args") [ "length" ]) Eq (Access (Var "f") [ "length" ]))
                        (Return <| Call (Var "f") [ Spread <| Var "args" ])
                        Nothing
                    , If (Binop (Access (Var "args") [ "length" ]) Gt (Access (Var "f") [ "length" ]))
                        (Block
                            [ Comment "This allows us to handle functions that have been explicitly curried,"
                            , Comment "or higher-order functions that return other functions."
                            , Comment ""
                            , Comment "If you supply more arguments than the wrapped function's arity, fully"
                            , Comment "call the function, wrap the result with $function, and then call *that*"
                            , Comment "function with the remaining arguments."
                            , Return <|
                                Call
                                    (Call (Var "$function")
                                        [ Call (Var "f")
                                            [ Spread
                                                (Call (Access (Var "args") [ "slice" ])
                                                    [ Number 0
                                                    , Access (Var "f") [ "length" ]
                                                    ]
                                                )
                                            ]
                                        , Var "thisArg"
                                        ]
                                    )
                                    [ Spread
                                        (Call (Access (Var "args") [ "slice" ])
                                            [ Access (Var "f") [ "length" ] ]
                                        )
                                    ]
                            ]
                        )
                        Nothing
                    , Return <|
                        Call
                            (Var "$function")
                            [ Call (Access (Var "f") [ "bind" ])
                                [ Var "thisArg"
                                , Spread (Var "args")
                                ]
                            , Var "thisArg"
                            ]
                    ]
            ]
    , Comment "Ren uses structural equality to compare objects and arrays. This"
    , Comment "is different to equality in JavaScript that is purely referential."
    , Comment "We need this utility to use in place of the usual `==` operator."

    --     function $eq (x, y) {
    --         const values = [x, y]
    --
    --         while (values.length > 0) {
    --             const a = values.pop()
    --             const b = values.pop()
    --
    --             if (a === b) continue
    --             if (a === null || a === undefined || b === null || b === undefined) return false
    --
    --             if (typeof a === 'object' || typeof b === 'object') {
    --                 if (a.valueOf() === b.valueOf()) continue
    --                 if (a.constructor !== b.constructor) return false
    --                 if (a.constructor === Date) {
    --                     if (!(a > b || a < b)) {
    --                         continue
    --                     } else {
    --                         return false
    --                     }
    --                 }
    --
    --                 for (const k in a) {
    --                     values.push(a[k], b[k])
    --                 }
    --
    --                 continue
    --             }
    --
    --             return false
    --         }
    --
    --         return true
    --     }
    , Export <|
        FunctionDeclaration "$eq" [ "x", "y" ] <|
            [ Const "eqs" <| Array [ Var "x", Var "y" ]
            , While (Binop (Access (Var "eqs") [ "length" ]) Gt (Number 0)) <|
                Block
                    [ Const "a" <| Call (Access (Var "eqs") [ "pop" ]) []
                    , Const "b" <| Call (Access (Var "eqs") [ "pop" ]) []
                    , If (Binop (Var "a") Eq (Var "b")) Continue Nothing
                    , If
                        (List.foldl (\lhs rhs -> Binop lhs Or rhs)
                            (Binop (Var "a") Eq Null)
                            [ Binop (Var "a") Eq Undefined
                            , Binop (Var "b") Eq Null
                            , Binop (Var "b") Eq Undefined
                            ]
                        )
                        (Return JSFalse)
                        Nothing
                    , If
                        (Binop (Binop (Unop Typeof (Var "a")) Eq (String "object")) Or (Binop (Unop Typeof (Var "b")) Eq (String "object")))
                        (Block
                            [ If (Binop (Call (Access (Var "a") [ "valueOf" ]) []) Eq (Call (Access (Var "b") [ "valueOf" ]) [])) Continue Nothing
                            , If (Binop (Access (Var "a") [ "constructor" ]) Neq (Access (Var "b") [ "constructor" ])) (Return JSFalse) Nothing
                            , If (Binop (Access (Var "a") [ "constructor" ]) Eq (Var "Date"))
                                (If (Unop Not (Binop (Binop (Var "a") Gt (Var "b")) Or (Binop (Var "a") Lt (Var "b"))))
                                    Continue
                                    (Just <| Return JSFalse)
                                )
                                Nothing
                            , ForIn "k" (Var "a") <|
                                Expr <|
                                    Call (Access (Var "eqs") [ "push" ]) [ Index (Var "a") (Var "k"), Index (Var "b") (Var "k") ]
                            , Continue
                            ]
                        )
                        Nothing
                    , Return JSFalse
                    ]
            , Return JSTrue
            ]
    ]



-- CONSTRUCTORS ----------------------------------------------------------------


fromExpr : Expr -> Statement
fromExpr =
    let
        branchFromCase arg ( pattern, guard, body ) =
            If (checksFromPattern (Var arg) pattern)
                (case guard of
                    Just (Expr g) ->
                        Block <|
                            List.concat
                                [ assignmentsFromPattern (Var arg) pattern
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
        , binop = \op lhs rhs -> fromOperator op lhs rhs
        , call = \fun args -> Expr <| Call (asExpression fun) (List.map asExpression args)
        , if_ = \cond then_ else_ -> Expr <| Ternary (asExpression cond) (asExpression then_) (asExpression <| else_)
        , lambda =
            \args body ->
                args
                    |> List.indexedMap Tuple.pair
                    |> List.foldr
                        (\( i, pattern ) ( names, checks, assignments ) ->
                            case pattern of
                                Pat.Var name ->
                                    ( name :: names, checks, assignments )

                                _ ->
                                    let
                                        name =
                                            "$" ++ String.fromInt i
                                    in
                                    ( name :: names
                                    , checksFromPattern (Var name) pattern :: checks
                                    , assignmentsFromPattern (Var name) pattern ++ assignments
                                    )
                        )
                        ( [], [], [] )
                    |> (\( names, checks, assignments ) ->
                            Call (Var "$function")
                                [ Function names <|
                                    case checks of
                                        check :: rest ->
                                            [ If (List.foldr (\a b -> Binop a And b) check rest)
                                                (return <| Block <| assignments ++ [ body ])
                                                Nothing
                                            , Throw "Non-exhaustive pattern match"
                                            ]

                                        [] ->
                                            [ return <| Block <| assignments ++ [ body ]
                                            ]
                                ]
                       )
                    |> Expr
        , let_ =
            \pattern stmt body ->
                case ( pattern, asExpression stmt ) of
                    ( Pat.Var name, expr ) ->
                        Block [ Const name expr, body ]

                    ( _, Var var ) ->
                        Block
                            [ return <| branchFromCase var ( pattern, Nothing, body )
                            , Throw "Non-exhaustive pattern match"
                            ]

                    ( _, expr ) ->
                        Block <|
                            [ Const "$pat" expr
                            , return <| branchFromCase "$pat" ( pattern, Nothing, body )
                            , Throw "Non-exhaustive pattern match"
                            ]
        , literal =
            \lit ->
                case lit of
                    Lit.Array elements ->
                        Expr <| Array <| List.map asExpression elements

                    Lit.Enum "true" [] ->
                        Expr <| JSTrue

                    Lit.Enum "false" [] ->
                        Expr <| JSFalse

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
        , scoped = \scope name -> Expr <| Var <| String.join "$" scope ++ "." ++ name
        , where_ =
            \stmt cases ->
                Block <|
                    List.concat
                        [ [ Const "$pat" <| asExpression stmt ]
                        , List.map (return << branchFromCase "$pat") cases
                        , [ Throw "Non-exhaustive pattern match" ]
                        ]
        , var = \var -> Expr <| Var var
        }


fromOperator : Op -> Statement -> Statement -> Statement
fromOperator op lhs rhs =
    case op of
        Op.Add ->
            Expr <| Binop (asExpression lhs) Add (asExpression rhs)

        Op.And ->
            Expr <| Binop (asExpression lhs) And (asExpression rhs)

        Op.Concat ->
            Expr <| Array [ Spread (asExpression lhs), Spread (asExpression rhs) ]

        Op.Cons ->
            Expr <| Array [ asExpression lhs, Spread (asExpression rhs) ]

        Op.Div ->
            Expr <| Binop (asExpression lhs) Div (asExpression rhs)

        Op.Eq ->
            Expr <| Call (Var "$eq") [ asExpression lhs, asExpression rhs ]

        Op.Gte ->
            Expr <| Binop (asExpression lhs) Gte (asExpression rhs)

        Op.Gt ->
            Expr <| Binop (asExpression lhs) Gt (asExpression rhs)

        Op.Lte ->
            Expr <| Binop (asExpression lhs) Lte (asExpression rhs)

        Op.Lt ->
            Expr <| Binop (asExpression lhs) Lt (asExpression rhs)

        Op.Mod ->
            Expr <| Binop (asExpression lhs) Mod (asExpression rhs)

        Op.Mul ->
            Expr <| Binop (asExpression lhs) Mul (asExpression rhs)

        Op.Neq ->
            Expr <| Unop Not <| Call (Var "$eq") [ asExpression lhs, asExpression rhs ]

        Op.Or ->
            Expr <| Binop (asExpression lhs) Or (asExpression rhs)

        Op.Pipe ->
            Expr <| Call (asExpression rhs) [ asExpression lhs ]

        Op.Seq ->
            Block [ lhs, rhs ]

        Op.Sub ->
            Expr <| Binop (asExpression lhs) Sub (asExpression rhs)


checksFromPattern : Expression -> Pat -> Expression
checksFromPattern expr pattern =
    case pattern of
        Pat.Any ->
            JSTrue

        Pat.Literal (Lit.Array elements) ->
            elements
                |> List.indexedMap (\i el -> checksFromPattern (Index expr <| Number <| Basics.toFloat i) el)
                -- Patterns like the wildcard `_` are always just true. This is
                -- necessary to handle top-level patterns that match on anything
                -- but once we're inside a container like an array we can just
                -- remove these checks altogether.
                |> List.filter ((/=) JSTrue)
                -- We also want to check the length of the array, if it's not long
                -- enough to satisfy all the other patterns, there's no point trying
                -- any of them!
                |> (::) (Binop (Access expr [ "length" ]) Gte (Number <| Basics.toFloat <| List.length elements))
                -- Finally, we'll do a runtime type check to confirm the value
                -- actually *is* an array. In JavaScript doing `typeof arr` will
                -- (perhaps unintuitively) return `"object"` for arrays, so we
                -- need to call a method on the global `Array` object instead.
                |> List.foldl (\y x -> Binop x And y) (Call (Access (Var "globalThis") [ "Array", "isArray" ]) [ expr ])

        Pat.Literal (Lit.Enum "true" []) ->
            Binop expr Eq JSTrue

        Pat.Literal (Lit.Enum "false" []) ->
            Binop expr Eq JSFalse

        Pat.Literal (Lit.Enum "undefined" []) ->
            Binop expr Eq Undefined

        Pat.Literal (Lit.Enum tag args) ->
            -- Variants are represented as arrays at runtime, where the first
            -- element is the tag and the rest are any arguments.
            checksFromPattern expr <| Pat.Literal <| Lit.Array <| (Pat.Literal (Lit.String tag) :: args)

        Pat.Literal (Lit.Number n) ->
            Binop expr Eq (Number n)

        Pat.Literal (Lit.Record fields) ->
            fields
                |> List.concatMap (\( k, v ) -> [ Binop (String k) In expr, checksFromPattern (Access expr [ k ]) v ])
                -- As with arrays, we can safely remove checks that will always
                -- succeed.
                |> List.filter ((/=) JSTrue)
                |> List.foldl (\y x -> Binop x And y) (Binop (Unop Typeof expr) Eq (String "object"))

        Pat.Literal (Lit.String s) ->
            Binop expr Eq (String s)

        Pat.Type "Array" pat ->
            Binop
                (Call (Access (Var "Array") [ "isArray" ]) [ expr ])
                And
                (checksFromPattern expr pat)

        Pat.Spread _ ->
            JSTrue

        Pat.Type type_ pat ->
            Binop
                (Binop
                    (Binop (Unop Typeof expr) Eq (String type_))
                    Or
                    (Binop (Access expr [ "constructor", "name" ]) Eq (String type_))
                )
                And
                (checksFromPattern expr pat)

        Pat.Var _ ->
            JSTrue


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

        Pat.Spread _ ->
            []

        Pat.Type _ pat ->
            assignmentsFromPattern expr pat

        Pat.Var name ->
            [ Const name expr ]



-- QUERIES ---------------------------------------------------------------------


{-| Used for determining when to wrap subexpressions in parentheses. Uses precedence
numbers from: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence>
-}
precedence : Expression -> Int
precedence expr =
    case expr of
        Access _ _ ->
            17

        Index _ _ ->
            17

        Unop New _ ->
            17

        Call _ _ ->
            17

        Unop Pos _ ->
            14

        Unop Neg _ ->
            14

        Unop Typeof _ ->
            14

        Binop _ Mul _ ->
            12

        Binop _ Div _ ->
            12

        Binop _ Mod _ ->
            12

        Binop _ Add _ ->
            11

        Binop _ Sub _ ->
            11

        Binop _ Lt _ ->
            9

        Binop _ Lte _ ->
            9

        Binop _ Gt _ ->
            9

        Binop _ Gte _ ->
            9

        Binop _ In _ ->
            9

        Binop _ Instanceof _ ->
            9

        Binop _ Eq _ ->
            8

        Binop _ Neq _ ->
            8

        Binop _ And _ ->
            4

        Binop _ Or _ ->
            3

        Binop _ Comma _ ->
            1

        -- The way we check if a current expression should be wrapped in parentheses,
        -- we look at the expression's precedence and compare it to the current
        -- precedence level. If it is lower, it get's wrapped.
        --
        -- Besides operators, other kinds of expression don't really need to deal
        -- with this so we say they have infinite precedence and never wrap.
        _ ->
            Util.Math.infinite


{-| Turn a single statement into a list of statements. For blocks this just
extracts the inner statements, for all others this produces a singleton list.
-}
statements : Statement -> List Statement
statements stmt =
    case stmt of
        Block stmts ->
            stmts

        _ ->
            [ stmt ]


{-| -}
isSimpleExpression : Expression -> Bool
isSimpleExpression expr =
    case expr of
        Access _ _ ->
            True

        Array _ ->
            False

        Assign _ _ ->
            False

        Binop _ _ _ ->
            False

        Call _ _ ->
            False

        Function _ _ ->
            False

        JSFalse ->
            True

        IIFE _ ->
            False

        Index _ _ ->
            True

        Null ->
            True

        Number _ ->
            True

        Object _ ->
            False

        Spread _ ->
            False

        String _ ->
            True

        Ternary _ _ _ ->
            False

        JSTrue ->
            True

        Undefined ->
            True

        Unop _ _ ->
            False

        Var _ ->
            True



-- MANIPULATIONS ---------------------------------------------------------------


{-| Flatten nested blocks where possible. This means if we produce an ast like:

    { const x = 1;
        { const y = 2;
            { return x + y }
        }
    }

it will flatten simply to:

    { const x = 1;
      const y = 2;
      return x + y
    }

-}
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

        Break ->
            Break

        Comment str ->
            Comment str

        Const _ expr ->
            Return expr

        Continue ->
            Continue

        Expr expr ->
            Return expr

        Export s ->
            return s

        FunctionDeclaration _ args body ->
            Return <| Function args body

        ForIn _ _ _ ->
            Block [ stmt, Return Undefined ]

        If cond then_ else_ ->
            If cond (return then_) (Maybe.map return else_)

        Return expr ->
            Return expr

        Throw error ->
            Throw error

        While _ _ ->
            Block [ stmt, Return Undefined ]


{-| Modifies the last statement in a list to be a return statement.
-}
returnLast : List Statement -> List Statement
returnLast stmts =
    case List.reverse stmts of
        stmt :: rest ->
            List.reverse <| return stmt :: rest

        [] ->
            []



-- CONVERSIONS -----------------------------------------------------------------


{-| Convert arbitrary statements to an expression by wrapping them in an IIFE if
necessary.
-}
asExpression : Statement -> Expression
asExpression stmt =
    case stmt of
        Const _ expr ->
            expr

        Expr expr ->
            expr

        FunctionDeclaration _ args body ->
            Function args body

        Return expr ->
            expr

        _ ->
            IIFE stmt



-- UTILS -----------------------------------------------------------------------
