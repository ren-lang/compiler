module Ren.IR.Imp exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Util.Json as Json



-- TYPES -----------------------------------------------------------------------


type Ast
    = Access Ast String
    | Block (List Ast)
    | Binary Binop Ast Ast
    | Bool Bool
    | Call Ast (List Ast)
    | Const String Ast
    | For Ast Ast Ast Ast
    | Function (Maybe String) (List String) Ast
    | If Ast Ast (Maybe Ast)
    | Index Ast Ast
    | Instanceof Ast Ast
    | Number Float
    | String String
    | Ternary Ast Ast Ast
    | Throw Ast
    | Typeof Ast String
    | Unary Unop Ast
    | While Ast Ast
    | Return (Maybe Ast)
    | Undefined
    | Var String
    | Array (List Ast)
    | Object (List ( String, Ast ))


type Unop
    = Neg
    | New
    | Not
    | Pos


type Binop
    = Add
    | And
    | Div
    | Eq
    | Gt
    | Gte
    | Lt
    | Lte
    | Mod
    | Mul
    | Neq
    | Or
    | Pow
    | Sub



-- JSON ------------------------------------------------------------------------


encode : Ast -> Json.Encode.Value
encode ast =
    case ast of
        Access expr key ->
            Json.taggedEncoder "Access"
                []
                [ encode expr
                , Json.Encode.string key
                ]

        Block stmts ->
            Json.taggedEncoder "Block"
                []
                [ Json.Encode.list encode stmts
                ]

        Binary binop lhs rhs ->
            Json.taggedEncoder "Binary"
                []
                [ encodeBinop binop
                , encode lhs
                , encode rhs
                ]

        Bool b ->
            Json.taggedEncoder "Bool" [] [ Json.Encode.bool b ]

        Call fun args ->
            Json.taggedEncoder "Call"
                []
                [ encode fun
                , Json.Encode.list encode args
                ]

        Const name expr ->
            Json.taggedEncoder "Const"
                []
                [ Json.Encode.string name
                , encode expr
                ]

        For init cond incr body ->
            Json.taggedEncoder "For"
                []
                [ encode init
                , encode cond
                , encode incr
                , encode body
                ]

        Function name args body ->
            Json.taggedEncoder "Function"
                []
                [ Maybe.withDefault Json.Encode.null <| Maybe.map Json.Encode.string name
                , Json.Encode.list Json.Encode.string args
                , encode body
                ]

        If cond true false ->
            Json.taggedEncoder "If"
                []
                [ encode cond
                , encode true
                , Maybe.withDefault Json.Encode.null <| Maybe.map encode false
                ]

        Index expr access ->
            Json.taggedEncoder "Index"
                []
                [ encode expr
                , encode access
                ]

        Instanceof expr class ->
            Json.taggedEncoder "Instanceof"
                []
                [ encode expr
                , encode class
                ]

        Number n ->
            Json.taggedEncoder "Number"
                []
                [ Json.Encode.float n
                ]

        String s ->
            Json.taggedEncoder "String"
                []
                [ Json.Encode.string s
                ]

        Ternary cond true false ->
            Json.taggedEncoder "Ternary"
                []
                [ encode cond
                , encode true
                , encode false
                ]

        Throw message ->
            Json.taggedEncoder "Throw"
                []
                [ encode message
                ]

        Typeof expr type_ ->
            Json.taggedEncoder "Typeof"
                []
                [ encode expr
                , Json.Encode.string type_
                ]

        Unary unop expr ->
            Json.taggedEncoder "Unary"
                []
                [ encodeUnop unop
                , encode expr
                ]

        While cond body ->
            Json.taggedEncoder "While"
                []
                [ encode cond
                , encode body
                ]

        Return value ->
            Json.taggedEncoder "Return"
                []
                [ Maybe.withDefault Json.Encode.null <| Maybe.map encode value
                ]

        Undefined ->
            Json.taggedEncoder "Undefined" [] []

        Var name ->
            Json.taggedEncoder "Var"
                []
                [ Json.Encode.string name
                ]

        Array elements ->
            Json.taggedEncoder "Array"
                []
                [ Json.Encode.list encode elements
                ]

        Object fields ->
            Json.taggedEncoder "Object"
                []
                [ Json.Encode.list
                    (\( name, expr ) ->
                        Json.Encode.list Basics.identity
                            [ Json.Encode.string name, encode expr ]
                    )
                    fields
                ]


encodeUnop : Unop -> Json.Encode.Value
encodeUnop unop =
    Json.Encode.string <|
        case unop of
            Neg ->
                "-"

            New ->
                "new"

            Not ->
                "!"

            Pos ->
                "+"


encodeBinop : Binop -> Json.Encode.Value
encodeBinop binop =
    Json.Encode.string <|
        case binop of
            Add ->
                "+"

            And ->
                "&&"

            Div ->
                "/"

            Eq ->
                "=="

            Gt ->
                ">"

            Gte ->
                ">="

            Lt ->
                "<"

            Lte ->
                "<="

            Mod ->
                "%"

            Mul ->
                "*"

            Neq ->
                "!="

            Or ->
                "||"

            Pow ->
                "**"

            Sub ->
                "-"


decoder : Json.Decode.Decoder Ast
decoder =
    let
        lazyDecoder =
            Json.Decode.lazy <| \_ -> decoder
    in
    Json.taggedDecoder
        (\ast ->
            case ast of
                "Access" ->
                    Json.Decode.map2 Access
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| Json.Decode.string)

                "Block" ->
                    Json.Decode.map Block
                        (Json.Decode.index 1 <| Json.Decode.list lazyDecoder)

                "Binary" ->
                    Json.Decode.map3 Binary
                        (Json.Decode.index 1 <| binopDecoder)
                        (Json.Decode.index 2 <| lazyDecoder)
                        (Json.Decode.index 3 <| lazyDecoder)

                "Bool" ->
                    Json.Decode.map Bool
                        (Json.Decode.index 1 <| Json.Decode.bool)

                "Call" ->
                    Json.Decode.map2 Call
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| Json.Decode.list lazyDecoder)

                "Const" ->
                    Json.Decode.map2 Const
                        (Json.Decode.index 1 <| Json.Decode.string)
                        (Json.Decode.index 2 <| lazyDecoder)

                "For" ->
                    Json.Decode.map4 For
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| lazyDecoder)
                        (Json.Decode.index 3 <| lazyDecoder)
                        (Json.Decode.index 4 <| lazyDecoder)

                "Function" ->
                    Json.Decode.map3 Function
                        (Json.Decode.index 1 <| Json.Decode.nullable Json.Decode.string)
                        (Json.Decode.index 2 <| Json.Decode.list Json.Decode.string)
                        (Json.Decode.index 3 <| lazyDecoder)

                "If" ->
                    Json.Decode.map3 If
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| lazyDecoder)
                        (Json.Decode.index 3 <| Json.Decode.nullable lazyDecoder)

                "Index" ->
                    Json.Decode.map2 Index
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| lazyDecoder)

                "Instanceof" ->
                    Json.Decode.map2 Instanceof
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| lazyDecoder)

                "Number" ->
                    Json.Decode.map Number
                        (Json.Decode.index 1 <| Json.Decode.float)

                "String" ->
                    Json.Decode.map String
                        (Json.Decode.index 1 <| Json.Decode.string)

                "Ternary" ->
                    Json.Decode.map3 Ternary
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| lazyDecoder)
                        (Json.Decode.index 3 <| lazyDecoder)

                "Throw" ->
                    Json.Decode.map Throw
                        (Json.Decode.index 1 <| lazyDecoder)

                "Typeof" ->
                    Json.Decode.map2 Typeof
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| Json.Decode.string)

                "Unary" ->
                    Json.Decode.map2 Unary
                        (Json.Decode.index 1 <| unopDecoder)
                        (Json.Decode.index 2 <| lazyDecoder)

                "Var" ->
                    Json.Decode.map Var
                        (Json.Decode.index 1 <| Json.Decode.string)

                "Array" ->
                    Json.Decode.map Array
                        (Json.Decode.index 1 <| Json.Decode.list lazyDecoder)

                "Object" ->
                    Json.Decode.map Object
                        (Json.Decode.index 1 <|
                            Json.Decode.list
                                (Json.Decode.map2 Tuple.pair
                                    (Json.Decode.index 0 <| Json.Decode.string)
                                    (Json.Decode.index 1 <| lazyDecoder)
                                )
                        )

                _ ->
                    Json.Decode.fail <| "Unknown AST tag: " ++ ast
        )


unopDecoder : Json.Decode.Decoder Unop
unopDecoder =
    Json.taggedDecoder
        (\unop ->
            case unop of
                "-" ->
                    Json.Decode.succeed Neg

                "new" ->
                    Json.Decode.succeed New

                "!" ->
                    Json.Decode.succeed Not

                "+" ->
                    Json.Decode.succeed Pos

                _ ->
                    Json.Decode.fail <| "Unknown unop: " ++ unop
        )


binopDecoder : Json.Decode.Decoder Binop
binopDecoder =
    Json.taggedDecoder
        (\binop ->
            case binop of
                "+" ->
                    Json.Decode.succeed Add

                "&&" ->
                    Json.Decode.succeed And

                "/" ->
                    Json.Decode.succeed Div

                "==" ->
                    Json.Decode.succeed Eq

                ">" ->
                    Json.Decode.succeed Gt

                ">=" ->
                    Json.Decode.succeed Gte

                "<" ->
                    Json.Decode.succeed Lt

                "<=" ->
                    Json.Decode.succeed Lte

                "%" ->
                    Json.Decode.succeed Mod

                "*" ->
                    Json.Decode.succeed Mul

                "!=" ->
                    Json.Decode.succeed Neq

                "||" ->
                    Json.Decode.succeed Or

                "**" ->
                    Json.Decode.succeed Pow

                "-" ->
                    Json.Decode.succeed Sub

                _ ->
                    Json.Decode.fail <| "Unknown binop: " ++ binop
        )
