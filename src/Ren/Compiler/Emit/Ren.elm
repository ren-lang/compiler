module Ren.Compiler.Emit.Ren exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Data.Either
import Pretty
import Ren.AST.Expr as Expr
    exposing
        ( Expr(..)
        , ExprF(..)
        , Identifier(..)
        , Pattern(..)
        )
import Ren.AST.Module as Module exposing (Module)
import Ren.Compiler.Emit.Util as Util
import Ren.Data.Type as Type


{-| -}
run : Module meta -> String
run =
    module_ >> Pretty.pretty 80



--                                                                            --
-- EMITTING MODULES ------------------------------------------------------------
--                                                                            --


module_ : Module meta -> Pretty.Doc t
module_ { imports, declarations } =
    if List.isEmpty imports then
        Pretty.join Util.doubleline (List.map declaration declarations)

    else
        Pretty.join Pretty.line (List.map import_ imports)
            |> Pretty.a Util.doubleline
            |> Pretty.a (Pretty.join Util.doubleline (List.map declaration declarations))


import_ : Module.Import -> Pretty.Doc t
import_ { path, name, exposed } =
    let
        alias_ =
            if List.isEmpty name then
                Pretty.empty

            else
                Pretty.string " as "
                    |> Pretty.a (Pretty.join (Pretty.char '.') <| List.map Pretty.string name)

        bindings =
            if List.isEmpty exposed then
                Pretty.empty

            else
                Pretty.string " exposing "
                    |> Pretty.a (Pretty.surround (Pretty.string "{ ") (Pretty.string " }") <| Pretty.join (Pretty.string ", ") <| List.map Pretty.string exposed)
    in
    Pretty.string "import "
        |> Pretty.a (Pretty.char '"')
        |> Pretty.a (Pretty.string path)
        |> Pretty.a (Pretty.char '"')
        |> Pretty.a alias_
        |> Pretty.a bindings


declaration : Module.Declaration meta -> Pretty.Doc t
declaration { public, name, type_, expr } =
    Pretty.string "let "
        |> Pretty.a (Pretty.string name)
        |> Pretty.a (Pretty.string " : ")
        |> Pretty.a (Pretty.string <| Type.toString type_)
        |> Pretty.a (Pretty.string " = ")
        |> Pretty.a Pretty.line
        |> Pretty.a (Expr.cata (always expression) expr)
        |> Pretty.append
            (if public then
                Pretty.string "pub "

             else
                Pretty.empty
            )
        |> Pretty.nest 4



--                                                                            --
-- EMITTING EXPRESSIONS --------------------------------------------------------
--


{-| -}
expression : ExprF (Pretty.Doc t) -> Pretty.Doc t
expression exprF =
    case exprF of
        Access expr accessors ->
            access expr accessors

        Application expr args ->
            application expr args

        Annotation expr t ->
            expr
                |> Pretty.a (Pretty.string " as ")
                |> Pretty.a (Pretty.string <| Type.toString t)
                |> Pretty.surround (Pretty.char '(') (Pretty.char ')')

        Block bindings expr ->
            block bindings expr

        Conditional cond true false ->
            conditional cond true false

        Error _ ->
            Pretty.empty

        Identifier id ->
            identifier id

        Infix op lhs rhs ->
            infix_ op lhs rhs

        Lambda args expr ->
            lambda args expr

        Literal lit ->
            literal lit

        Match expr cases ->
            match expr cases


{-| -}
access : Pretty.Doc t -> List String -> Pretty.Doc t
access expr accessors =
    expr
        |> Pretty.a (Pretty.char '.')
        |> Pretty.a (Pretty.join (Pretty.char '.') <| List.map Pretty.string accessors)


{-| -}
application : Pretty.Doc t -> List (Pretty.Doc t) -> Pretty.Doc t
application expr args =
    expr
        |> Pretty.a Pretty.space
        |> Pretty.a (Pretty.join Pretty.space args)


{-| -}
block : List ( String, Pretty.Doc t ) -> Pretty.Doc t -> Pretty.Doc t
block bindings expr =
    let
        binding ( name, body ) =
            Pretty.string "let "
                |> Pretty.a (Pretty.string name)
                |> Pretty.a (Pretty.string " = ")
                |> Pretty.a body
    in
    Pretty.char '{'
        |> Pretty.a Pretty.line
        |> Pretty.a
            (List.map binding bindings
                |> Pretty.join Pretty.line
                |> Pretty.a Pretty.line
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.string "ret ")
                |> Pretty.a expr
                |> Pretty.indent 4
            )
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.char '}')


{-| -}
conditional : Pretty.Doc t -> Pretty.Doc t -> Pretty.Doc t -> Pretty.Doc t
conditional cond true false =
    Pretty.string "if "
        |> Pretty.a cond
        |> Pretty.a (Pretty.string " then ")
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.indent 4 true)
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.string "else ")
        |> Pretty.a (Pretty.indent 4 false)


{-| -}
identifier : Expr.Identifier -> Pretty.Doc t
identifier id =
    case id of
        Expr.Local name ->
            Pretty.string name

        Expr.Scoped namespace name ->
            Pretty.string namespace
                |> Pretty.a (Pretty.char '.')
                |> Pretty.a (identifier name)

        Expr.Placeholder Nothing ->
            Pretty.char '_'

        Expr.Placeholder (Just name) ->
            Pretty.char '_'
                |> Pretty.a (Pretty.string name)


{-| -}
infix_ : Expr.Operator -> Pretty.Doc t -> Pretty.Doc t -> Pretty.Doc t
infix_ op lhs rhs =
    case op of
        Expr.Pipe ->
            Util.splitBinop "|>" lhs rhs

        Expr.Compose ->
            Util.binop ">>" lhs rhs

        Expr.Add ->
            Util.binop "+" lhs rhs

        Expr.Sub ->
            Util.binop "-" lhs rhs

        Expr.Mul ->
            Util.binop "*" lhs rhs

        Expr.Div ->
            Util.binop "/" lhs rhs

        Expr.Pow ->
            Util.binop "^" lhs rhs

        Expr.Mod ->
            Util.binop "%" lhs rhs

        Expr.Eq ->
            Util.binop "==" lhs rhs

        Expr.NotEq ->
            Util.binop "!=" lhs rhs

        Expr.Lt ->
            Util.binop "<" lhs rhs

        Expr.Lte ->
            Util.binop "<=" lhs rhs

        Expr.Gt ->
            Util.binop ">" lhs rhs

        Expr.Gte ->
            Util.binop ">=" lhs rhs

        Expr.And ->
            Util.binop "&&" lhs rhs

        Expr.Or ->
            Util.binop "||" lhs rhs

        Expr.Cons ->
            Util.binop "::" lhs rhs

        Expr.Join ->
            Util.binop "++" lhs rhs


{-| -}
lambda : List Expr.Pattern -> Pretty.Doc t -> Pretty.Doc t
lambda args expr =
    List.map pattern args
        |> Pretty.join Pretty.space
        |> Pretty.a (Pretty.string " => ")
        |> Pretty.a expr


literal : Expr.Literal (Pretty.Doc t) -> Pretty.Doc t
literal lit =
    case lit of
        Expr.Array elements ->
            Util.array elements

        Expr.Boolean True ->
            Pretty.string "true"

        Expr.Boolean False ->
            Pretty.string "false"

        Expr.Number n ->
            String.fromFloat n
                |> Pretty.string

        Expr.Record entries ->
            Util.object entries

        Expr.String text ->
            Util.string <| Pretty.string text

        Expr.Template segments ->
            segments
                |> List.map (Data.Either.extract Pretty.string (Pretty.surround (Pretty.string "${ ") (Pretty.string " }")))
                |> Pretty.join Pretty.empty
                |> Pretty.surround (Pretty.char '`') (Pretty.char '`')

        Expr.Undefined ->
            Pretty.string "()"

        Expr.Variant tag [] ->
            Pretty.char '#'
                |> Pretty.a (Pretty.string tag)

        Expr.Variant tag args ->
            Pretty.char '#'
                |> Pretty.a (Pretty.string tag)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.join Pretty.space args)


match : Pretty.Doc t -> List ( Expr.Pattern, Maybe (Pretty.Doc t), Pretty.Doc t ) -> Pretty.Doc t
match expr cases =
    let
        case_ ( pat, guard, body ) =
            Pretty.string "is "
                |> Pretty.a (pattern pat)
                |> Pretty.a (Maybe.map (Pretty.append (Pretty.string " if ")) guard |> Maybe.withDefault Pretty.empty)
                |> Pretty.a (Pretty.string " => ")
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.indent 4 body)
    in
    Pretty.string "where "
        |> Pretty.a expr
        |> Pretty.a
            (List.map case_ cases
                |> Pretty.join (Pretty.a Pretty.line Pretty.line)
                |> Pretty.append Pretty.line
            )
        |> Pretty.nest 4


pattern : Expr.Pattern -> Pretty.Doc t
pattern pat =
    case pat of
        Expr.ArrayDestructure elements ->
            Util.array <| List.map pattern elements

        Expr.LiteralPattern (Expr.Array _) ->
            Pretty.empty

        Expr.LiteralPattern (Expr.Boolean True) ->
            Pretty.string "true"

        Expr.LiteralPattern (Expr.Boolean False) ->
            Pretty.string "false"

        Expr.LiteralPattern (Expr.Number n) ->
            String.fromFloat n
                |> Pretty.string

        Expr.LiteralPattern (Expr.Record _) ->
            Pretty.empty

        Expr.LiteralPattern (Expr.String s) ->
            Util.string <| Pretty.string s

        Expr.LiteralPattern (Expr.Template _) ->
            Pretty.empty

        Expr.LiteralPattern Expr.Undefined ->
            Pretty.string "()"

        Expr.LiteralPattern (Expr.Variant _ _) ->
            Pretty.empty

        Expr.Name name ->
            Pretty.string name

        Expr.RecordDestructure entries ->
            let
                entry ( k, p ) =
                    Pretty.string k
                        |> Pretty.a (Maybe.map (pattern >> Pretty.append (Pretty.string ": ")) p |> Maybe.withDefault Pretty.empty)
            in
            List.map entry entries
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.surround (Pretty.string "{ ") (Pretty.string " }")

        Expr.Spread name ->
            Pretty.string "..."
                |> Pretty.a (Pretty.string name)

        Expr.Typeof type_ p ->
            Pretty.char '@'
                |> Pretty.a (Pretty.string type_)
                |> Pretty.a Pretty.space
                |> Pretty.a (pattern p)

        Expr.VariantDestructure tag [] ->
            Pretty.char '#'
                |> Pretty.a (Pretty.string tag)

        Expr.VariantDestructure tag ps ->
            Pretty.char '#'
                |> Pretty.a (Pretty.string tag)
                |> Pretty.a Pretty.space
                |> Pretty.a (Pretty.join Pretty.space <| List.map pattern ps)

        Expr.Wildcard Nothing ->
            Pretty.char '_'

        Expr.Wildcard (Just name) ->
            Pretty.char '_'
                |> Pretty.a (Pretty.string name)
