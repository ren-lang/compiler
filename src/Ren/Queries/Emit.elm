module Ren.Queries.Emit exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Pretty
import Ren.Ast.Decl as Decl exposing (Decl)
import Ren.Ast.Decl.Ext exposing (Ext(..))
import Ren.Ast.Decl.Imp as Imp exposing (Imp(..))
import Ren.Ast.Decl.Let exposing (Let(..))
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Ast.Mod as Mod exposing (Mod)
import Ren.Ast.Type as Type
import Ren.Control.Query as Query exposing (Query)
import Ren.IR.Imp as Imp
import Ren.Queries.Parse as Parse
import Util.Math as Math
import Util.Pretty as Pretty



--


prelude : String
prelude =
    List.map fromStmt Imp.prelude
        |> Pretty.join Pretty.line
        |> Pretty.pretty 80


mod : String -> String -> String -> Query { env | modules : Dict String Mod } String
mod pkgs name path =
    Query.do (Parse.file path) <| \m ->
    Query.succeed <| Pretty.pretty 80 <| fromMod pkgs name m


decl : String -> Decl -> String
decl pkgs d =
    fromDecl pkgs d
        |> Pretty.pretty 80


expr : Expr -> String
expr e =
    Expr.desugar e
        |> Imp.fromExpr
        |> fromStmt
        |> Pretty.pretty 80



--


fromMod : String -> String -> Mod -> Pretty.Doc ()
fromMod pkgs name m =
    let
        preludeImport =
            concat
                [ Pretty.string "import"
                , Pretty.space
                , Pretty.string "{ $eq, $function }"
                , Pretty.space
                , Pretty.string "from"
                , Pretty.space
                , Pretty.char '\''
                , Pretty.string <| pkgs ++ "/prelude.js"
                , Pretty.char '\''
                ]

        ffiImport =
            concat
                [ Pretty.string "import"
                , Pretty.space
                , Pretty.char '*'
                , Pretty.space
                , Pretty.string "as"
                , Pretty.space
                , Pretty.string "$FFI"
                , Pretty.space
                , Pretty.string "from"
                , Pretty.space
                , Pretty.char '\''
                , Pretty.string <| "./" ++ name ++ ".ffi.js"
                , Pretty.char '\''
                ]
    in
    Pretty.join Pretty.line <|
        [ preludeImport
        , when (Basics.not <| Dict.isEmpty m.externals) ffiImport
        , Pretty.join Pretty.doubleline <| (List.map (List.map (fromDecl pkgs) >> Pretty.join Pretty.line) <| Mod.emittables m)
        ]


fromDecl : String -> Decl -> Pretty.Doc ()
fromDecl pkgs d =
    let
        withDocs docs =
            when (Basics.not <| List.isEmpty docs)
                (List.map (String.replace "#" "//" >> Pretty.string) docs
                    |> Pretty.join Pretty.line
                )

        withType name t =
            concat
                [ Pretty.string "// "
                , Pretty.string name
                , Pretty.string " : "
                , Pretty.string <| Type.toString t
                ]
    in
    case d of
        Decl.Let (Let meta pub name _ e) ->
            case Imp.fromExpr <| Expr.desugar e of
                Imp.Expr (Imp.Function args body) ->
                    concat
                        [ withDocs meta.docs
                        , Pretty.line
                        , Pretty.string "//"
                        , Pretty.line
                        , withType name meta.tipe
                        , Pretty.line
                        , when pub (Pretty.string "export")
                        , when pub Pretty.space
                        , fromStmt <| Imp.FunctionDeclaration name args body
                        , Pretty.line
                        ]

                Imp.Expr expr_ ->
                    concat
                        [ withDocs meta.docs
                        , Pretty.line
                        , Pretty.string "//"
                        , Pretty.line
                        , withType name meta.tipe
                        , Pretty.line
                        , when pub (Pretty.string "export")
                        , when pub Pretty.space
                        , fromStmt <| Imp.Const name expr_
                        , Pretty.line
                        ]

                stmt ->
                    concat
                        [ withDocs meta.docs
                        , Pretty.line
                        , Pretty.string "//"
                        , Pretty.line
                        , withType name meta.tipe
                        , Pretty.line
                        , when pub (Pretty.string "export")
                        , when pub Pretty.space
                        , fromStmt <| Imp.Const name <| Imp.asExpression <| Imp.return <| Imp.flatten stmt
                        , Pretty.line
                        ]

        Decl.Ext (Ext meta pub name _ s) ->
            concat
                [ withDocs meta.docs
                , Pretty.line
                , Pretty.string "//"
                , Pretty.line
                , withType name meta.tipe
                , Pretty.line
                , when pub (Pretty.string "export")
                , when pub Pretty.space
                , fromStmt <| Imp.Const name <| Imp.Call (Imp.Var "$function") [ Imp.Access (Imp.Var "$FFI") [ s ] ]
                , Pretty.line
                ]

        Decl.Comment _ comments ->
            List.map (String.replace "#" "//" >> Pretty.string) comments
                |> Pretty.join Pretty.line
                |> Pretty.a Pretty.line

        Decl.Type _ ->
            Pretty.empty

        Decl.Imp (Imp _ path source name) ->
            concat
                [ Pretty.string "import"
                , Pretty.space
                , Pretty.char '*'
                , Pretty.space
                , when (Basics.not <| List.isEmpty name)
                    (concat
                        [ Pretty.string "as"
                        , Pretty.space
                        , Pretty.string <| String.join "$" name
                        , Pretty.space
                        , Pretty.string "from"
                        , Pretty.space
                        ]
                    )
                , Pretty.char '\''
                , Pretty.string
                    (if source == Imp.Package then
                        pkgs ++ "/" ++ path ++ ".ren.js"

                     else if source == Imp.Relative then
                        path ++ ".ren.js"

                     else
                        path
                    )
                , Pretty.char '\''
                ]


fromStmt : Imp.Statement -> Pretty.Doc ()
fromStmt stmt =
    case stmt of
        Imp.Block _ ->
            block stmt

        Imp.Break ->
            Pretty.string "break"

        Imp.Comment cmt ->
            concat
                [ Pretty.string "//"
                , Pretty.space
                , Pretty.string cmt
                ]

        Imp.Const name e ->
            concat
                [ Pretty.string "const"
                , Pretty.space
                , Pretty.string name
                , Pretty.space
                , Pretty.char '='
                , Pretty.space
                , fromExpression e
                ]

        Imp.Continue ->
            Pretty.string "continue"

        Imp.Expr e ->
            fromExpression e

        Imp.Export s ->
            concat
                [ Pretty.string "export"
                , Pretty.space
                , fromStmt s
                ]

        Imp.FunctionDeclaration name args body ->
            concat
                [ Pretty.string "function"
                , Pretty.space
                , Pretty.string name
                , Pretty.space
                , Pretty.char '('
                , Pretty.join (Pretty.char ',') <| List.map Pretty.string args
                , Pretty.char ')'
                , Pretty.space
                , block <| Imp.Block body
                ]

        Imp.ForIn var e body ->
            concat
                [ Pretty.string "for"
                , Pretty.space
                , Pretty.char '('
                , Pretty.string "var"
                , Pretty.space
                , Pretty.string var
                , Pretty.space
                , Pretty.string "in"
                , Pretty.space
                , fromExpression e
                , Pretty.char ')'
                , Pretty.space
                , block <| Imp.Block [ body ]
                ]

        Imp.If cond then_ else_ ->
            concat
                [ Pretty.string "if"
                , Pretty.space
                , Pretty.parens <| fromExpression cond
                , Pretty.space
                , block then_
                , case else_ of
                    Just stmt_ ->
                        concat
                            [ Pretty.space
                            , Pretty.string "else"
                            , Pretty.space
                            , block stmt_
                            ]

                    Nothing ->
                        Pretty.empty
                ]

        Imp.Return e ->
            concat
                [ Pretty.string "return"
                , Pretty.space
                , fromExpression e
                ]

        Imp.Throw error ->
            concat
                [ Pretty.string "throw"
                , Pretty.space
                , Pretty.string "new Error"
                , Pretty.parens <| Pretty.surround (Pretty.char '`') (Pretty.char '`') <| Pretty.string error
                ]

        Imp.While cond body ->
            concat
                [ Pretty.string "while"
                , Pretty.space
                , Pretty.parens <| fromExpression cond
                , Pretty.space
                , block body
                ]


fromExpression : Imp.Expression -> Pretty.Doc ()
fromExpression e =
    let
        precedence =
            Imp.precedence e
    in
    case e of
        Imp.Access obj [] ->
            fromExpression obj

        Imp.Access obj keys ->
            concat
                [ parenthesise precedence obj
                , Pretty.char '.'
                , Pretty.join (Pretty.char '.') (List.map Pretty.string keys)
                ]

        Imp.Assign lhs rhs ->
            concat
                [ fromExpression lhs
                , Pretty.space
                , Pretty.char '='
                , Pretty.space
                , fromExpression rhs
                ]

        Imp.Binop x Imp.Add y ->
            binop precedence x "+" y

        Imp.Binop x Imp.And y ->
            binop precedence x "&&" y

        Imp.Array elements ->
            concat
                [ Pretty.char '['
                , List.map fromExpression elements
                    |> Pretty.join (Pretty.string ", ")
                , Pretty.char ']'
                ]

        Imp.Function args body ->
            concat
                [ Pretty.char '('
                , Pretty.join (Pretty.string ", ") (List.map Pretty.string args)
                , Pretty.char ')'
                , Pretty.space
                , Pretty.string "=>"
                , Pretty.space
                , case body of
                    [ Imp.Expr ((Imp.Object _) as expr_) ] ->
                        concat
                            [ Pretty.char '('
                            , fromExpression expr_
                            , Pretty.char ')'
                            ]

                    [ Imp.Expr expr_ ] ->
                        fromExpression expr_

                    [ Imp.Return expr_ ] ->
                        fromExpression expr_

                    [ Imp.Block [ Imp.Return expr_ ] ] ->
                        fromExpression expr_

                    _ ->
                        block <| Imp.flatten <| Imp.Block body
                ]

        Imp.JSTrue ->
            Pretty.string "true"

        Imp.JSFalse ->
            Pretty.string "false"

        Imp.Call ((Imp.Function _ _) as fun) args ->
            concat
                [ Pretty.parens <| fromExpression fun
                , Pretty.parens <| Pretty.join (Pretty.string ", ") <| List.map fromExpression args
                ]

        Imp.Call fun args ->
            concat
                [ fromExpression fun
                , Pretty.parens <| Pretty.join (Pretty.string ", ") <| List.map fromExpression args
                ]

        Imp.Binop x Imp.Comma y ->
            binop precedence x "," y

        Imp.Binop x Imp.Div y ->
            binop precedence x "/" y

        Imp.Binop x Imp.Eq y ->
            binop precedence x "===" y

        Imp.Binop x Imp.Gt y ->
            binop precedence x ">" y

        Imp.Binop x Imp.Gte y ->
            binop precedence x ">=" y

        Imp.Binop x Imp.In y ->
            binop precedence x "in" y

        Imp.Binop x Imp.Instanceof y ->
            binop precedence x "instanceof" y

        Imp.Binop x Imp.Lt y ->
            binop precedence x "<" y

        Imp.Binop x Imp.Lte y ->
            binop precedence x "<=" y

        Imp.Binop x Imp.Mod y ->
            binop precedence x "%" y

        Imp.Binop x Imp.Mul y ->
            binop precedence x "*" y

        Imp.Binop x Imp.Neq y ->
            binop precedence x "!==" y

        Imp.Binop x Imp.Or y ->
            binop precedence x "||" y

        Imp.Binop x Imp.Sub y ->
            binop precedence x "-" y

        Imp.IIFE stmt ->
            concat
                [ Pretty.char '('
                , Pretty.string "()"
                , Pretty.space
                , Pretty.string "=>"
                , Pretty.space
                , fromStmt stmt
                , Pretty.char ')'
                , Pretty.string "()"
                ]

        Imp.Index expr_ idx ->
            concat
                [ parenthesise precedence expr_
                , Pretty.char '['
                , fromExpression idx
                , Pretty.char ']'
                ]

        Imp.Null ->
            Pretty.string "null"

        Imp.Number n ->
            Pretty.string <| String.fromFloat n

        Imp.Object fields ->
            concat
                [ Pretty.char '{'
                , fields
                    |> List.map
                        (\( k, v ) ->
                            if Imp.Var k == v then
                                fromExpression v

                            else
                                concat
                                    [ Pretty.string k
                                    , Pretty.char ':'
                                    , Pretty.space
                                    , fromExpression v
                                    ]
                        )
                    |> Pretty.join (Pretty.string ", ")
                , Pretty.char '}'
                ]

        Imp.Spread expr_ ->
            concat
                [ Pretty.string "..."
                , if Imp.precedence expr_ == Math.infinite then
                    fromExpression expr_

                  else
                    Pretty.parens <| fromExpression expr_
                ]

        Imp.String s ->
            concat
                [ Pretty.string "`"
                , Pretty.string s
                , Pretty.string "`"
                ]

        Imp.Ternary cond then_ else_ ->
            concat
                [ parenthesise precedence cond
                , Pretty.space
                , Pretty.string "?"
                , fromExpression then_
                , Pretty.space
                , Pretty.string ":"
                , Pretty.space
                , fromExpression else_
                ]

        Imp.Unop Imp.Typeof expr_ ->
            concat
                [ Pretty.string "typeof"
                , Pretty.space
                , parenthesise precedence expr_
                ]

        Imp.Unop Imp.Neg expr_ ->
            concat
                [ Pretty.string "-"
                , parenthesise precedence expr_
                ]

        Imp.Unop Imp.New expr_ ->
            concat
                [ Pretty.string "new"
                , Pretty.space
                , parenthesise precedence expr_
                ]

        Imp.Unop Imp.Not expr_ ->
            concat
                [ Pretty.string "!"
                , parenthesise precedence expr_
                ]

        Imp.Unop Imp.Pos expr_ ->
            concat
                [ Pretty.string "+"
                , parenthesise precedence expr_
                ]

        Imp.Undefined ->
            Pretty.string "undefined"

        Imp.Var name ->
            Pretty.string name



-- QUERIES ---------------------------------------------------------------------
-- MANIPULATIONS ---------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------


block : Imp.Statement -> Pretty.Doc ()
block stmt =
    let
        withSpacing s =
            case s of
                Imp.Block _ ->
                    concat [ Pretty.line, fromStmt s ]

                Imp.Break ->
                    fromStmt s

                Imp.Comment _ ->
                    fromStmt s

                Imp.Const _ _ ->
                    fromStmt s

                Imp.Continue ->
                    fromStmt s

                Imp.Expr _ ->
                    fromStmt s

                Imp.Export stmt_ ->
                    fromStmt stmt_

                Imp.FunctionDeclaration _ _ _ ->
                    concat [ Pretty.line, fromStmt s ]

                Imp.ForIn _ _ _ ->
                    concat [ Pretty.line, fromStmt s ]

                Imp.If _ _ _ ->
                    concat [ Pretty.line, fromStmt s ]

                Imp.Return _ ->
                    concat [ Pretty.line, fromStmt s ]

                Imp.Throw _ ->
                    concat [ Pretty.line, fromStmt s ]

                Imp.While _ _ ->
                    concat [ Pretty.line, fromStmt s ]

        statements =
            case Imp.statements <| Imp.flatten stmt of
                s :: rest ->
                    Pretty.indent 4 <|
                        Pretty.join Pretty.line <|
                            (fromStmt s :: List.map withSpacing rest)

                [] ->
                    Pretty.empty
    in
    concat
        [ Pretty.char '{'
        , Pretty.line
        , statements
        , Pretty.line
        , Pretty.char '}'
        ]


doubleline : Pretty.Doc ()
doubleline =
    Pretty.line |> Pretty.a Pretty.line


binop : Int -> Imp.Expression -> String -> Imp.Expression -> Pretty.Doc ()
binop precedence lhs op rhs =
    concat
        [ parenthesise precedence lhs
        , Pretty.space
        , Pretty.string op
        , Pretty.space
        , parenthesise precedence rhs
        ]


parenthesise : Int -> Imp.Expression -> Pretty.Doc ()
parenthesise precedence e =
    if precedence > Imp.precedence e then
        Pretty.parens <| fromExpression e

    else
        fromExpression e


when : Bool -> Pretty.Doc () -> Pretty.Doc ()
when true doc =
    if true then
        doc

    else
        Pretty.empty


concat : List (Pretty.Doc ()) -> Pretty.Doc ()
concat =
    List.foldl Pretty.a Pretty.empty
