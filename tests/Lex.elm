module Lex exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import Ren.AST.Expr as Expr
import Ren.AST.Token as Token
import Ren.Compiler.Lex exposing (run)
import Ren.Data.Span as Span
import Test exposing (..)


positionSuite : Test
positionSuite =
    describe "position for maths operations"
        [ test "addition with spaces" <|
            \_ ->
                run "123 + 234"
                    |> Result.map (List.map Tuple.first)
                    |> Expect.equal
                        (Ok
                            [ Span.fromTuples ( 1, 1 ) ( 1, 4 )
                            , Span.fromTuples ( 1, 5 ) ( 1, 6 )
                            , Span.fromTuples ( 1, 7 ) ( 1, 10 )
                            ]
                        )
        , test "addition without spaces" <|
            \_ ->
                run "23+3939"
                    |> Result.map (List.map Tuple.first)
                    |> Expect.equal
                        (Ok
                            [ Span.fromTuples ( 1, 1 ) ( 1, 3 )
                            , Span.fromTuples ( 1, 3 ) ( 1, 4 )
                            , Span.fromTuples ( 1, 4 ) ( 1, 8 )
                            ]
                        )
        , test "invalid addition" <|
            \_ ->
                run "+ 12 +"
                    |> Result.map (List.map Tuple.first)
                    |> Expect.equal
                        (Ok
                            [ Span.fromTuples ( 1, 1 ) ( 1, 2 )
                            , Span.fromTuples ( 1, 3 ) ( 1, 5 )
                            , Span.fromTuples ( 1, 6 ) ( 1, 7 )
                            ]
                        )
        ]


lexSuite : Test
lexSuite =
    describe "literals and operators"
        [ describe "tokens of maths operations"
            [ test "addition with spaces" <|
                \_ ->
                    run "123 + 234"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Token.Number 123
                                , Token.Operator Expr.Add
                                , Token.Number 234
                                ]
                            )
            , test "addition without spaces" <|
                \_ ->
                    run "23+3939"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Token.Number 23
                                , Token.Operator Expr.Add
                                , Token.Number 3939
                                ]
                            )
            , test "invalid addition" <|
                \_ ->
                    run "+ 12 +"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Token.Operator Expr.Add
                                , Token.Number 12
                                , Token.Operator Expr.Add
                                ]
                            )
            ]
        , describe "negative numbers and subtraction"
            [ test "subtracting positives" <|
                \_ ->
                    run "523 - 234"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Token.Number 523
                                , Token.Operator Expr.Sub
                                , Token.Number 234
                                ]
                            )
            , test "subtracting negatives" <|
                \_ ->
                    run "-523 - -234"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Token.Number -523
                                , Token.Operator Expr.Sub
                                , Token.Number -234
                                ]
                            )
            , test "adding negatives" <|
                \_ ->
                    run "-523 + -234"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Token.Number -523
                                , Token.Operator Expr.Add
                                , Token.Number -234
                                ]
                            )
            , test "invalid positive number notation" <|
                \_ ->
                    run "+523 - +234"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Token.Operator Expr.Add
                                , Token.Number 523
                                , Token.Operator Expr.Sub
                                , Token.Operator Expr.Add
                                , Token.Number 234
                                ]
                            )
            ]
        ]
