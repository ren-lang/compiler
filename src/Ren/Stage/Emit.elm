module Ren.Stage.Emit exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Pretty
import Ren.Ast.JavaScript as JavaScript exposing (precedence)
import Ren.Data.Declaration as Declaration exposing (Declaration)
import Ren.Data.Import exposing (Import)
import Ren.Data.Module exposing (Module)
import Util.Math



-- TYPES -----------------------------------------------------------------------


{-| The `Pretty.Doc` type has a type variable `t` used to tag strings in a document
for more fine-tuned rendering. We don't need any of that so instead of carrying
round a pointless type variable we'll just fill it in as `()` and use this alias
instead.
-}
type alias Doc =
    Pretty.Doc ()



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


fromModule : Module -> Doc
fromModule mod =
    mod.declarations
        |> List.map fromDeclaration
        |> Pretty.join doubleline


fromImport : Import -> Doc
fromImport imp =
    Debug.todo ""


fromDeclaration : Declaration -> Doc
fromDeclaration dec =
    case dec of
        Declaration.Let pub name expr ->
            Pretty.empty
                |> Pretty.a (when pub <| Pretty.string "export ")
                |> Pretty.a
                    (case JavaScript.fromExpr expr of
                        JavaScript.Expr (JavaScript.Arrow arg body) ->
                            Pretty.empty
                                |> Pretty.a (Pretty.string "function")
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.string name)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '(')
                                |> Pretty.a (Pretty.string arg)
                                |> Pretty.a (Pretty.char ')')
                                |> Pretty.a Pretty.space
                                |> Pretty.a (block body)

                        _ ->
                            Pretty.empty
                                |> Pretty.a (Pretty.string "const")
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.string name)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.string "=")
                                |> Pretty.a Pretty.space
                                |> Pretty.a (fromStatement <| JavaScript.fromExpr expr)
                    )

        Declaration.Ext pub name str ->
            Pretty.empty
                |> Pretty.a (when pub <| Pretty.string "export ")
                |> Pretty.a (Pretty.string "const")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string name)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '=')
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "$FFI")
                |> Pretty.a (Pretty.char '.')
                |> Pretty.a (Pretty.string str)


fromStatement : JavaScript.Statement -> Doc
fromStatement stmt =
    case stmt of
        JavaScript.Block _ ->
            block stmt

        JavaScript.Comment cmt ->
            Pretty.empty
                |> Pretty.a (Pretty.string "//")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string cmt)

        JavaScript.Const name expr ->
            Pretty.empty
                |> Pretty.a (Pretty.string "const")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string name)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.char '=')
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression expr)

        JavaScript.Expr expr ->
            fromExpression expr

        JavaScript.If cond then_ else_ ->
            Pretty.empty
                |> Pretty.a (Pretty.string "if")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.parens <| fromExpression cond)
                |> Pretty.a Pretty.space
                |> Pretty.a (block then_)
                |> Pretty.a
                    (case else_ of
                        Just stmt_ ->
                            Pretty.empty
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.string "else")
                                |> Pretty.a Pretty.space
                                |> Pretty.a (block stmt_)

                        Nothing ->
                            Pretty.empty
                    )

        JavaScript.Return expr ->
            Pretty.empty
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.string "return")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromExpression expr)

        JavaScript.Throw error ->
            Pretty.empty
                |> Pretty.a (Pretty.string "throw")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "new Error")
                |> Pretty.a (Pretty.parens <| Pretty.surround (Pretty.char '`') (Pretty.char '`') <| Pretty.string error)


fromExpression : JavaScript.Expression -> Doc
fromExpression expr =
    let
        precedence =
            JavaScript.precedence expr
    in
    case expr of
        JavaScript.Access expr_ [] ->
            fromExpression expr_

        JavaScript.Access expr_ keys ->
            Pretty.empty
                |> Pretty.a (parenthesise precedence expr_)
                |> Pretty.a (Pretty.char '.')
                |> Pretty.a (Pretty.join (Pretty.char '.') (List.map Pretty.string keys))

        JavaScript.Add x y ->
            binop precedence x "+" y

        JavaScript.And x y ->
            binop precedence x "&&" y

        JavaScript.Array elements ->
            Pretty.empty
                |> Pretty.a (Pretty.char '[')
                |> Pretty.a
                    (List.map fromExpression elements
                        |> Pretty.join (Pretty.string ", ")
                    )
                |> Pretty.a (Pretty.char ']')

        JavaScript.Arrow arg body ->
            Pretty.empty
                |> Pretty.a (Pretty.char '(')
                |> Pretty.a (Pretty.string arg)
                |> Pretty.a (Pretty.char ')')
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "=>")
                |> Pretty.a Pretty.space
                |> Pretty.a
                    (case body of
                        JavaScript.Return expr_ ->
                            fromExpression expr_

                        _ ->
                            block body
                    )

        JavaScript.Bool True ->
            Pretty.string "true"

        JavaScript.Bool False ->
            Pretty.string "false"

        JavaScript.Call fun args ->
            Pretty.empty
                |> Pretty.a (fromExpression fun)
                |> Pretty.a (Pretty.join Pretty.empty <| List.map (Pretty.parens << fromExpression) args)

        JavaScript.Div x y ->
            binop precedence x "/" y

        JavaScript.Eq x y ->
            binop precedence x "==" y

        JavaScript.Gt x y ->
            binop precedence x ">" y

        JavaScript.Gte x y ->
            binop precedence x ">=" y

        JavaScript.IIFE Nothing stmt ->
            Pretty.empty
                |> Pretty.a (Pretty.char '(')
                |> Pretty.a (Pretty.string "()")
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "=>")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromStatement stmt)
                |> Pretty.a (Pretty.char ')')
                |> Pretty.a (Pretty.string "()")

        JavaScript.IIFE (Just ( name, expr_ )) stmt ->
            Pretty.empty
                |> Pretty.a (Pretty.char '(')
                |> Pretty.a (Pretty.parens <| Pretty.string name)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.string "=>")
                |> Pretty.a Pretty.space
                |> Pretty.a (fromStatement stmt)
                |> Pretty.a (Pretty.char ')')
                |> Pretty.a (Pretty.parens <| fromExpression expr_)

        JavaScript.Index expr_ idx ->
            Pretty.empty
                |> Pretty.a (parenthesise precedence expr_)
                |> Pretty.a (Pretty.char '[')
                |> Pretty.a (fromExpression idx)
                |> Pretty.a (Pretty.char ']')

        JavaScript.Lt x y ->
            binop precedence x "<" y

        JavaScript.Lte x y ->
            binop precedence x "<=" y

        JavaScript.Mod x y ->
            binop precedence x "%" y

        JavaScript.Mul x y ->
            binop precedence x "*" y

        JavaScript.Neq x y ->
            binop precedence x "!=" y

        JavaScript.Number n ->
            Pretty.string <| String.fromFloat n

        JavaScript.Object fields ->
            Pretty.empty
                |> Pretty.a (Pretty.char '{')
                |> Pretty.a
                    (fields
                        |> List.map
                            (\( k, v ) ->
                                if JavaScript.Var k == v then
                                    fromExpression v

                                else
                                    Pretty.empty
                                        |> Pretty.a (Pretty.string k)
                                        |> Pretty.a Pretty.space
                                        |> Pretty.a (Pretty.char ':')
                                        |> Pretty.a Pretty.space
                                        |> Pretty.a (fromExpression v)
                            )
                        |> Pretty.join (Pretty.string ", ")
                    )
                |> Pretty.a (Pretty.char '}')

        JavaScript.Or x y ->
            binop precedence x "||" y

        JavaScript.Spread expr_ ->
            Pretty.empty
                |> Pretty.a (Pretty.string "...")
                |> Pretty.a
                    (if JavaScript.precedence expr_ == Util.Math.infinite then
                        fromExpression expr_

                     else
                        Pretty.parens <| fromExpression expr_
                    )

        JavaScript.String s ->
            Pretty.empty
                |> Pretty.a (Pretty.string "`")
                |> Pretty.a (Pretty.string s)
                |> Pretty.a (Pretty.string "`")

        JavaScript.Sub x y ->
            binop precedence x "-" y

        JavaScript.Typeof expr_ ->
            Pretty.empty
                |> Pretty.a (Pretty.string "typeof")
                |> Pretty.a Pretty.space
                |> Pretty.a (parenthesise precedence expr_)

        JavaScript.Undefined ->
            Pretty.string "undefined"

        JavaScript.Var name ->
            Pretty.string name



-- QUERIES ---------------------------------------------------------------------


statements : JavaScript.Statement -> List JavaScript.Statement
statements stmt =
    case stmt of
        JavaScript.Block stmts ->
            stmts

        _ ->
            [ stmt ]



-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------


emit : Int -> Module -> String
emit width mod =
    Pretty.pretty width <| fromModule mod



-- UTILS -----------------------------------------------------------------------


block : JavaScript.Statement -> Doc
block stmt =
    Pretty.empty
        |> Pretty.a (Pretty.char '{')
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.join Pretty.line <| List.map fromStatement <| statements stmt)
        |> Pretty.nest 4
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.char '}')


doubleline : Doc
doubleline =
    Pretty.line
        |> Pretty.a Pretty.line


binop : Int -> JavaScript.Expression -> String -> JavaScript.Expression -> Doc
binop precedence lhs op rhs =
    Pretty.empty
        |> Pretty.a (parenthesise precedence lhs)
        |> Pretty.a Pretty.space
        |> Pretty.a (Pretty.string op)
        |> Pretty.a Pretty.space
        |> Pretty.a (parenthesise precedence rhs)


parenthesise : Int -> JavaScript.Expression -> Doc
parenthesise precedence expr =
    if precedence > JavaScript.precedence expr then
        Pretty.parens <| fromExpression expr

    else
        fromExpression expr


when : Bool -> Doc -> Doc
when true doc =
    if true then
        doc

    else
        Pretty.empty
