module Ren.Stage.Emit.JavaScript exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Pretty
import Ren.Ast.Decl as Decl exposing (Decl)
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Ast.Mod as Mod exposing (Mod(..))
import Ren.Ast.Mod.Import as Import exposing (Import)
import Ren.Ast.Type as Type
import Ren.IR.Imp as Imp
import Util.Math as Math



--


prelude : String
prelude =
    List.map fromStmt Imp.prelude
        |> Pretty.join Pretty.line
        |> Pretty.pretty 80


mod : Mod -> String
mod m =
    fromMod m
        |> Pretty.pretty 80


decl : Decl -> String
decl d =
    fromDecl d
        |> Pretty.pretty 80


expr : Expr -> String
expr e =
    Expr.desugar e
        |> Imp.fromExpr
        |> fromStmt
        |> Pretty.pretty 80



--


fromMod : Mod -> Pretty.Doc ()
fromMod (Mod meta imports declarations) =
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
                , Pretty.string <| "./" ++ meta.pkgPath ++ "/prelude.js"
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
                , Pretty.string <| "./" ++ meta.name ++ ".ffi.js"
                , Pretty.char '\''
                ]
    in
    Pretty.join doubleline <|
        [ imports
            |> List.map (fromImport meta)
            |> (::) (when meta.usesFFI ffiImport)
            |> (::) preludeImport
            |> Pretty.join Pretty.line
        , declarations
            |> List.map fromDecl
            |> Pretty.join doubleline
        ]


fromImport : Mod.Meta -> Import -> Pretty.Doc ()
fromImport meta imp =
    let
        path =
            if Import.isPackage imp then
                meta.pkgPath ++ imp.path ++ ".ren.js"

            else
                imp.path ++ ".ren.js"
    in
    case ( imp.name, imp.unqualified ) of
        ( [], [] ) ->
            concat
                [ Pretty.string "import"
                , Pretty.space
                , Pretty.char '\''
                , Pretty.string path
                , Pretty.char '\''
                ]

        ( name, [] ) ->
            concat
                [ Pretty.string "import"
                , Pretty.space
                , Pretty.char '*'
                , Pretty.space
                , Pretty.string "as"
                , Pretty.space
                , Pretty.string <| String.join "$" name
                , Pretty.space
                , Pretty.string "from"
                , Pretty.space
                , Pretty.char '\''
                , Pretty.string path
                , Pretty.char '\''
                ]

        ( [], unqualified ) ->
            concat
                [ Pretty.string "import"
                , Pretty.space
                , Pretty.char '{'
                , Pretty.join (Pretty.char ',') <| List.map Pretty.string unqualified
                , Pretty.char '}'
                , Pretty.space
                , Pretty.string "from"
                , Pretty.space
                , Pretty.char '\''
                , Pretty.string path
                , Pretty.char '\''
                ]

        -- Because of the way Imp imports work, we'll need two separate
        -- import statements if we want to qualify an entire module under a specific
        -- name *and* introduce some unqualified bindings.
        --
        -- To save on duplication, we'll just call the `fromImport` again but
        -- clear out the `unqualified` and `name` fields respectively to emit
        -- just one import statement on each line.
        ( _, _ ) ->
            Pretty.join Pretty.line
                [ fromImport meta { imp | unqualified = [] }
                , fromImport meta { imp | name = [] }
                ]


fromDecl : Decl -> Pretty.Doc ()
fromDecl d =
    case d of
        Decl.Let meta pub name e ->
            case Imp.fromExpr <| Expr.desugar e of
                Imp.Expr (Imp.Function args body) ->
                    concat
                        [ Pretty.string "// "
                        , Pretty.string name
                        , Pretty.string " : "
                        , Pretty.string <| Type.toString meta.tipe
                        , Pretty.line
                        , when pub (Pretty.string "export")
                        , when pub Pretty.space
                        , fromStmt <| Imp.FunctionDeclaration name args body
                        ]

                Imp.Expr expr_ ->
                    concat
                        [ Pretty.string "// "
                        , Pretty.string name
                        , Pretty.string " : "
                        , Pretty.string <| Type.toString meta.tipe
                        , Pretty.line
                        , when pub (Pretty.string "export")
                        , when pub Pretty.space
                        , fromStmt <| Imp.Const name expr_
                        ]

                stmt ->
                    concat
                        [ Pretty.string "// "
                        , Pretty.string name
                        , Pretty.string " : "
                        , Pretty.string <| Type.toString meta.tipe
                        , Pretty.line
                        , when pub (Pretty.string "export")
                        , when pub Pretty.space
                        , fromStmt <| Imp.Const name <| Imp.asExpression <| Imp.return <| Imp.flatten stmt
                        ]

        Decl.Ext meta pub name str ->
            concat
                [ Pretty.string "// "
                , Pretty.string name
                , Pretty.string " : "
                , Pretty.string <| Type.toString meta.tipe
                , Pretty.line
                , when pub (Pretty.string "export")
                , when pub Pretty.space
                , fromStmt <| Imp.Const name <| Imp.Call (Imp.Var "$function") [ Imp.Access (Imp.Var "$FFI") [ str ] ]
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
                    concat [ fromStmt s ]

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
