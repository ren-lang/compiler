module Ren.Compiler.Emit.ESModule exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Data.Either
import Pretty
import Regex
import Ren.AST.Expr as Expr
    exposing
        ( Expr(..)
        , ExprF(..)
        , Identifier(..)
        , Pattern(..)
        )
import Ren.AST.Module as Module exposing (ImportSpecifier(..), Module)
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
module_ { name, imports, declarations } =
    if List.isEmpty imports then
        Pretty.join Util.doubleline (List.map declaration declarations)

    else
        Pretty.join Pretty.line (List.map (import_ name) imports)
            |> Pretty.a Util.doubleline
            |> Pretty.a (Pretty.join Util.doubleline (List.map declaration declarations))


import_ : String -> Module.Import -> Pretty.Doc t
import_ moduleName { path, name, exposed } =
    case ( name, exposed ) of
        ( [], [] ) ->
            Pretty.string "import "
                |> Pretty.a (Pretty.char '"')
                |> Pretty.a (importSpecifier moduleName path)
                |> Pretty.a (Pretty.char '"')

        ( parts, [] ) ->
            Pretty.string "import * as "
                |> Pretty.a (Pretty.join (Pretty.char '$') <| List.map Pretty.string parts)
                |> Pretty.a (Pretty.string " from ")
                |> Pretty.a (Pretty.char '"')
                |> Pretty.a (importSpecifier moduleName path)
                |> Pretty.a (Pretty.char '"')

        ( [], bindings ) ->
            Pretty.string "import "
                |> Pretty.a (Pretty.braces <| Pretty.join (Pretty.string ", ") <| List.map Pretty.string bindings)
                |> Pretty.a (Pretty.string " from ")
                |> Pretty.a (Pretty.char '"')
                |> Pretty.a (importSpecifier moduleName path)
                |> Pretty.a (Pretty.char '"')

        ( parts, bindings ) ->
            Pretty.join Pretty.line
                [ import_ moduleName { path = path, name = parts, exposed = [] }
                , import_ moduleName { path = path, name = [], exposed = bindings }
                ]


importSpecifier : String -> Module.ImportSpecifier -> Pretty.Doc t
importSpecifier moduleName specifier =
    case specifier of
        ExternalImport path ->
            Pretty.string path

        PackageImport path ->
            Pretty.string path

        LocalImport path ->
            Pretty.string path

        FfiImport ->
            Pretty.string <| moduleName ++ ".ffi.js"


declaration : Module.Declaration meta -> Pretty.Doc t
declaration declr =
    case declr of
        Module.Ext _ name _ _ ->
            Pretty.string <| "// Internal Error! Trying to emit an external declrattion: " ++ name

        Module.Let pub name type_ expr _ ->
            Pretty.string "// "
                |> Pretty.a (Pretty.string name)
                |> Pretty.a (Pretty.string " :: ")
                |> Pretty.a (Pretty.string <| Type.toString type_)
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (case expr of
                        Expr _ (Lambda [ arg ] body) ->
                            Pretty.string "function "
                                |> Pretty.a (Pretty.string name)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.parens <| lambdaPattern arg)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return "
                                        |> Pretty.a (wrapBuilder <| Expr.cata (always expression) body)
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.append
                                    (if pub then
                                        Pretty.string "export "

                                     else
                                        Pretty.empty
                                    )

                        Expr meta (Lambda (arg :: args) body) ->
                            Pretty.string "function "
                                |> Pretty.a (Pretty.string name)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.parens <| lambdaPattern arg)
                                |> Pretty.a Pretty.space
                                |> Pretty.a (Pretty.char '{')
                                |> Pretty.a Pretty.line
                                |> Pretty.a
                                    (Pretty.string "return "
                                        |> Pretty.a (wrapBuilder <| Expr.cata (always expression) <| Expr meta (Lambda args body))
                                        |> Pretty.indent 4
                                    )
                                |> Pretty.a Pretty.line
                                |> Pretty.a (Pretty.char '}')
                                |> Pretty.append
                                    (if pub then
                                        Pretty.string "export "

                                     else
                                        Pretty.empty
                                    )

                        _ ->
                            Pretty.string "const "
                                |> Pretty.a (Pretty.string name)
                                |> Pretty.a (Pretty.string " = ")
                                |> Pretty.a (wrapBuilder <| Expr.cata (always expression) expr)
                                |> Pretty.append
                                    (if pub then
                                        Pretty.string "export "

                                     else
                                        Pretty.empty
                                    )
                    )

        Module.Run expr _ ->
            Expr.cata (always expression) expr
                |> .expr



--                                                                            --
-- EMITTING EXPRESSIONS --------------------------------------------------------
--                                                                            --


{-| -}
type alias Builder t =
    { wrap : Pretty.Doc t -> Pretty.Doc t
    , expr : Pretty.Doc t
    }


wrapBuilder : Builder t -> Pretty.Doc t
wrapBuilder { wrap, expr } =
    wrap expr


{-| -}
expression : ExprF (Builder t) -> Builder t
expression exprF =
    case exprF of
        Access expr accessors ->
            access expr accessors

        Application expr args ->
            application expr args

        Annotation expr _ ->
            expr

        Block bindings expr ->
            block bindings expr

        Conditional cond true false ->
            conditional cond true false

        Error _ ->
            { wrap = Basics.identity, expr = Pretty.empty }

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



-- EMITTING EXPRESSIONS: ACCESS ------------------------------------------------


{-| -}
access : Builder t -> List String -> Builder t
access { wrap, expr } accessors =
    { wrap = Basics.identity
    , expr =
        Pretty.join (Pretty.char '.')
            (wrap expr :: List.map Pretty.string accessors)
    }



-- EMITTING EXPRESSIONS: APPLICATION -------------------------------------------


{-| -}
application : Builder t -> List (Builder t) -> Builder t
application { wrap, expr } args =
    { wrap = Pretty.parens
    , expr =
        Pretty.join (Pretty.char ' ')
            (wrap expr :: List.map (.expr >> Pretty.parens) args)
    }



-- EMITTING EXPRESSIONS: BLOCK -------------------------------------------------


{-| -}
block : List ( String, Builder t ) -> Builder t -> Builder t
block bindings { wrap, expr } =
    let
        binding b =
            case b of
                ( "_", gen ) ->
                    gen.expr

                ( name, gen ) ->
                    Pretty.join (Pretty.char ' ')
                        [ Pretty.string "const"
                        , Pretty.string name
                        , Pretty.char '='
                        , gen.expr
                        ]
    in
    if List.isEmpty bindings then
        { wrap = wrap, expr = expr }

    else
        { wrap = iife ( "", Pretty.empty )
        , expr =
            Pretty.char '{'
                |> Pretty.a Pretty.line
                |> Pretty.a
                    (List.map binding bindings
                        |> Pretty.join Pretty.line
                        |> Pretty.a Pretty.line
                        |> Pretty.a Pretty.line
                        |> Pretty.a (Pretty.string "return ")
                        |> Pretty.a expr
                        |> Pretty.indent 4
                    )
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.char '}')
        }



-- EMITTING EXPRESSIONS: CONDITIONAL -------------------------------------------


{-| -}
conditional : Builder t -> Builder t -> Builder t -> Builder t
conditional cond true false =
    { wrap = Pretty.parens
    , expr =
        Pretty.join (Pretty.char ' ')
            [ cond.expr
            , Pretty.char '?'
            , true.expr
            , Pretty.char ':'
            , false.expr
            ]
    }



-- EMITTING EXPRESSIONS: IDENTIFIER --------------------------------------------


{-| -}
identifier : Expr.Identifier -> Builder t
identifier id =
    case id of
        Expr.Local var ->
            { wrap = Basics.identity
            , expr = Pretty.string var
            }

        Expr.Scoped namespace ((Expr.Scoped _ _) as id_) ->
            { wrap = Basics.identity
            , expr =
                Pretty.string namespace
                    |> Pretty.a (Pretty.char '$')
                    |> Pretty.a (identifier id_).expr
            }

        Expr.Scoped namespace id_ ->
            { wrap = Basics.identity
            , expr =
                Pretty.string namespace
                    |> Pretty.a (Pretty.char '.')
                    |> Pretty.a (identifier id_).expr
            }

        Expr.Placeholder _ ->
            { wrap = Basics.identity
            , expr = Pretty.empty
            }



-- EMITTING EXPRESSIONS: INFIX -------------------------------------------------


{-| -}
infix_ : Expr.Operator -> Builder t -> Builder t -> Builder t
infix_ op lhs rhs =
    let
        binop s =
            { wrap = Pretty.parens
            , expr =
                Pretty.join (Pretty.string <| " " ++ s ++ " ")
                    [ lhs.wrap lhs.expr
                    , rhs.wrap rhs.expr
                    ]
            }
    in
    case op of
        Expr.Pipe ->
            application rhs [ lhs ]

        Expr.Compose ->
            lambda [ Expr.Name "$compose" ]
                (application rhs [ application lhs [ identifier (Expr.Local "$compose") ] ])

        Expr.Add ->
            binop "+"

        Expr.Sub ->
            binop "-"

        Expr.Mul ->
            binop "*"

        Expr.Div ->
            binop "/"

        Expr.Pow ->
            binop "**"

        Expr.Mod ->
            binop "%"

        Expr.Eq ->
            binop "=="

        Expr.NotEq ->
            binop "!="

        Expr.Lt ->
            binop "<"

        Expr.Lte ->
            binop "<="

        Expr.Gt ->
            binop ">"

        Expr.Gte ->
            binop ">="

        Expr.And ->
            binop "&&"

        Expr.Or ->
            binop "||"

        Expr.Cons ->
            { wrap = Pretty.parens
            , expr =
                [ lhs.expr
                , Pretty.string "..." |> Pretty.a (rhs.wrap rhs.expr)
                ]
                    |> Pretty.join (Pretty.string ", ")
                    |> Pretty.brackets
            }

        Expr.Join ->
            { wrap = Pretty.parens
            , expr =
                [ Pretty.string "..." |> Pretty.a (lhs.wrap lhs.expr)
                , Pretty.string "..." |> Pretty.a (rhs.wrap rhs.expr)
                ]
                    |> Pretty.join (Pretty.string ", ")
                    |> Pretty.brackets
            }



-- EMITTING EXPRESSIONS: LAMBDA ------------------------------------------------


{-| -}
lambda : List Expr.Pattern -> Builder t -> Builder t
lambda args { wrap, expr } =
    { wrap = Pretty.parens
    , expr =
        List.map (lambdaPattern >> Pretty.parens) args
            |> Pretty.join (Pretty.string " => ")
            |> Pretty.a (Pretty.string " => ")
            |> Pretty.a (wrap expr)
    }


{-| -}
lambdaPattern : Expr.Pattern -> Pretty.Doc t
lambdaPattern pat =
    case pat of
        Expr.ArrayDestructure elements ->
            List.map lambdaPattern elements
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.brackets

        Expr.LiteralPattern _ ->
            Pretty.char '_'

        Expr.Name name ->
            Pretty.string name

        Expr.RecordDestructure entries ->
            let
                entry ( name, maybePattern ) =
                    case maybePattern of
                        Just (Expr.Spread _) ->
                            Pretty.string "..."
                                |> Pretty.a (Pretty.string name)

                        Just p ->
                            Pretty.string name
                                |> Pretty.a (Pretty.string ": ")
                                |> Pretty.a (lambdaPattern p)

                        Nothing ->
                            Pretty.string name
            in
            List.map entry entries
                |> Pretty.join (Pretty.string ", ")
                |> Pretty.braces

        Expr.Spread name ->
            Pretty.string "..."
                |> Pretty.a (Pretty.string name)

        Expr.TemplateDestructure _ ->
            Pretty.string "_"

        Expr.Typeof _ _ ->
            Pretty.string "_"

        Expr.VariantDestructure tag patterns ->
            lambdaPattern (Expr.ArrayDestructure <| Expr.Name ("#" ++ tag) :: patterns)

        Expr.Wildcard name ->
            Pretty.string "_"
                |> Pretty.a (Maybe.map Pretty.string name |> Maybe.withDefault Pretty.empty)



-- EMITTING EXPRESSIONS: LITERAL -----------------------------------------------


{-| -}
literal : Expr.Literal (Builder t) -> Builder t
literal lit =
    case lit of
        Expr.Array elements ->
            { wrap = Basics.identity
            , expr =
                Pretty.brackets <|
                    Pretty.join
                        (Pretty.string ", ")
                        (List.map .expr elements)
            }

        Expr.Boolean True ->
            { wrap = Basics.identity
            , expr = Pretty.string "true"
            }

        Expr.Boolean False ->
            { wrap = Basics.identity
            , expr = Pretty.string "false"
            }

        Expr.Number n ->
            { wrap = Basics.identity
            , expr = Pretty.string <| String.fromFloat n
            }

        Expr.Record entries ->
            let
                entry ( key, { expr } ) =
                    Pretty.string key
                        |> Pretty.a (Pretty.string ": ")
                        |> Pretty.a expr
            in
            { wrap = Pretty.parens
            , expr =
                Pretty.braces <|
                    Pretty.join
                        (Pretty.string ", ")
                        (List.map entry entries)
            }

        Expr.String s ->
            { wrap = Basics.identity
            , expr =
                Pretty.surround
                    (Pretty.char '"')
                    (Pretty.char '"')
                    (Pretty.string s)
            }

        Expr.Template segments ->
            let
                segment =
                    Data.Either.extract
                        Pretty.string
                        (.expr >> Pretty.surround (Pretty.string "${") (Pretty.string "}"))
            in
            { wrap = Basics.identity
            , expr =
                Pretty.join Pretty.empty (List.map segment segments)
                    |> Pretty.surround (Pretty.char '`') (Pretty.char '`')
            }

        Expr.Undefined ->
            { wrap = Basics.identity
            , expr = Pretty.string "undefined"
            }

        Expr.Variant name args ->
            { wrap = Basics.identity
            , expr =
                Pretty.join
                    (Pretty.string ", ")
                    (Pretty.string ("\"$" ++ name ++ "\"") :: List.map .expr args)
                    |> Pretty.brackets
            }



-- EMITTING EXPRESSIONS: MATCH -------------------------------------------------


{-| -}
match : Builder t -> List ( Expr.Pattern, Maybe (Builder t), Builder t ) -> Builder t
match { expr } cases =
    let
        isVariable =
            Regex.fromString "^[a-z][a-zA-Z0-9_]*$" |> Maybe.withDefault Regex.never

        exprString =
            Pretty.pretty 0 expr

        matchVariable =
            if Regex.contains isVariable exprString then
                exprString

            else
                "$match"
    in
    { wrap = Basics.identity
    , expr =
        Pretty.char '{'
            |> Pretty.a Pretty.line
            |> Pretty.a
                (List.map (matchCase matchVariable) cases
                    |> Pretty.join Pretty.line
                    |> Pretty.a Pretty.line
                    |> Pretty.a Pretty.line
                    |> Pretty.a (Pretty.string "throw new Error(\"Incomplete pattern match.\")")
                    |> Pretty.indent 4
                )
            |> Pretty.a Pretty.line
            |> Pretty.a (Pretty.char '}')
            |> iife ( matchVariable, expr )
    }


{-| -}
matchCase : String -> ( Expr.Pattern, Maybe (Builder t), Builder t ) -> Pretty.Doc t
matchCase name ( pat, guard, { expr } ) =
    Pretty.string "if ("
        |> Pretty.a (matchPattern name pat)
        |> Pretty.a
            (Maybe.map (.expr >> Pretty.append (Pretty.string " && ")) guard
                |> Maybe.withDefault Pretty.empty
            )
        |> Pretty.a (Pretty.string ") {")
        |> Pretty.a Pretty.line
        |> Pretty.a
            ((case matchBindings name pat of
                [] ->
                    Pretty.empty

                bindings ->
                    Pretty.join Pretty.line bindings
                        |> Pretty.a Pretty.line
                        |> Pretty.a Pretty.line
             )
                |> Pretty.a (Pretty.string "return ")
                |> Pretty.a expr
                |> Pretty.indent 4
            )
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.char '}')


{-| -}
matchPattern : String -> Expr.Pattern -> Pretty.Doc t
matchPattern name pat =
    case pat of
        Expr.ArrayDestructure elements ->
            Pretty.join (Pretty.string " && ")
                [ Pretty.string name
                    |> Pretty.a (Pretty.string ".length >= ")
                    |> Pretty.a (Pretty.string <| String.fromInt <| List.length elements)
                , Pretty.join (Pretty.string " && ")
                    (List.indexedMap
                        (\i el -> matchPattern (name ++ "[" ++ String.fromInt i ++ "]") el)
                        elements
                    )
                ]

        -- Array literal patterns are impossible, that's what the ArrayDestructure
        -- pattern is for!
        Expr.LiteralPattern (Expr.Array _) ->
            Pretty.empty

        Expr.LiteralPattern (Expr.Boolean True) ->
            Pretty.string name
                |> Pretty.a (Pretty.string " === ")
                |> Pretty.a (Pretty.string "true")

        Expr.LiteralPattern (Expr.Boolean False) ->
            Pretty.string name
                |> Pretty.a (Pretty.string " === ")
                |> Pretty.a (Pretty.string "false")

        Expr.LiteralPattern (Expr.Number n) ->
            Pretty.string name
                |> Pretty.a (Pretty.string " === ")
                |> Pretty.a (Pretty.string <| String.fromFloat n)

        -- Record literal patterns are impossible, that's what the RecordDestructure
        -- pattern is for!
        Expr.LiteralPattern (Expr.Record _) ->
            Pretty.empty

        Expr.LiteralPattern (Expr.String s) ->
            Pretty.string name
                |> Pretty.a (Pretty.string " === ")
                |> Pretty.a (Pretty.surround (Pretty.char '"') (Pretty.char '"') <| Pretty.string s)

        -- You might have noticed that patterns can't contain expressions (that
        -- wouldn't be a pattern, silly!). That rules template literals out of
        -- the equation too then.
        --
        -- Fun fact: it's actually impossible to construct these sorts of patterns
        -- because their type is `LiteralPattern (Literal Never)`.
        Expr.LiteralPattern (Expr.Template _) ->
            Pretty.empty

        Expr.LiteralPattern Expr.Undefined ->
            Pretty.join (Pretty.string " || ")
                [ Pretty.string name
                    |> Pretty.a (Pretty.string " === ")
                    |> Pretty.a (Pretty.string "undefined")
                , Pretty.string name
                    |> Pretty.a (Pretty.string " === ")
                    |> Pretty.a (Pretty.string "null")
                ]
                |> Pretty.parens

        -- Record literal patterns are impossible, that's what the Variantestructure
        -- pattern is for!
        Expr.LiteralPattern (Expr.Variant _ _) ->
            Pretty.empty

        -- `Name` patterns introduce bindings but don't involve any checking, so
        -- we don't need to emit anything here.
        Expr.Name _ ->
            Pretty.empty

        Expr.RecordDestructure entries ->
            Pretty.join (Pretty.string " && ")
                (List.map
                    (\( key, val ) ->
                        case val of
                            Just p ->
                                Pretty.join (Pretty.string " && ")
                                    [ Pretty.string key
                                        |> Pretty.surround (Pretty.char '"') (Pretty.char '"')
                                        |> Pretty.a (Pretty.string " in ")
                                        |> Pretty.a (Pretty.string name)
                                    , matchPattern (name ++ "." ++ key) p
                                    ]

                            Nothing ->
                                Pretty.string key
                                    |> Pretty.surround (Pretty.char '"') (Pretty.char '"')
                                    |> Pretty.a (Pretty.string " in ")
                                    |> Pretty.a (Pretty.string name)
                    )
                    entries
                )

        -- Like `Name` patterns, `Spread` patterns only introduce bindings.
        Expr.Spread _ ->
            Pretty.empty

        Expr.TemplateDestructure segments ->
            Pretty.string "new RegExp('^"
                |> Pretty.a (Pretty.string <| String.join "" <| List.map (Data.Either.extract escapeRegex (Basics.always "(.*)")) segments)
                |> Pretty.a (Pretty.string "$').test(")
                |> Pretty.a (Pretty.string name)
                |> Pretty.a (Pretty.string ")")

        Expr.Typeof "Array" p ->
            Pretty.join (Pretty.string " && ")
                [ Pretty.string "Array.isArray("
                    |> Pretty.a (Pretty.string name)
                    |> Pretty.a (Pretty.string ")")
                , matchPattern name p
                ]

        Expr.Typeof "Boolean" p ->
            Pretty.join (Pretty.string " && ")
                [ Pretty.string "typeof "
                    |> Pretty.a (Pretty.string name)
                    |> Pretty.a (Pretty.string "=== \"boolean\"")
                , matchPattern name p
                ]

        Expr.Typeof "Number" p ->
            Pretty.join (Pretty.string " && ")
                [ Pretty.string "typeof "
                    |> Pretty.a (Pretty.string name)
                    |> Pretty.a (Pretty.string "=== \"number\"")
                , matchPattern name p
                ]

        Expr.Typeof "Object" p ->
            Pretty.join (Pretty.string " && ")
                [ Pretty.string "typeof "
                    |> Pretty.a (Pretty.string name)
                    |> Pretty.a (Pretty.string "=== \"object\"")
                , matchPattern name p
                ]

        Expr.Typeof "String" p ->
            Pretty.join (Pretty.string " && ")
                [ Pretty.string "typeof "
                    |> Pretty.a (Pretty.string name)
                    |> Pretty.a (Pretty.string "=== \"string\"")
                , matchPattern name p
                ]

        Expr.Typeof typeName p ->
            Pretty.join (Pretty.string " && ")
                [ Pretty.string name
                    |> Pretty.a (Pretty.string " instanceof ")
                    |> Pretty.a (Pretty.string typeName)
                , matchPattern name p
                ]

        Expr.VariantDestructure tagName ps ->
            matchPattern name <|
                Expr.ArrayDestructure (Expr.LiteralPattern (Expr.String <| "$" ++ tagName) :: ps)

        -- Wildcard patterns don't introduce bindings *or* involve any checking:
        -- nothing to emit at all! They're still very useful to have though, with
        -- a wildcard you could pattern match on an array of three elements but
        -- only care about the first and last by doing:
        --
        -- ```
        -- is [ first, _, last ] => ...
        -- ```
        --
        Expr.Wildcard _ ->
            Pretty.string "true"


{-| -}
matchBindings : String -> Expr.Pattern -> List (Pretty.Doc t)
matchBindings name pat =
    case pat of
        Expr.ArrayDestructure elements ->
            List.indexedMap
                (\i el -> matchBindings (name ++ "[" ++ String.fromInt i ++ "]") el)
                elements
                |> List.concat

        Expr.LiteralPattern _ ->
            []

        Expr.Name bindingName ->
            [ Pretty.string "const "
                |> Pretty.a (Pretty.string bindingName)
                |> Pretty.a (Pretty.string " = ")
                |> Pretty.a (Pretty.string name)
            ]

        Expr.RecordDestructure entries ->
            List.map
                (\( key, val ) ->
                    Maybe.map (matchBindings (name ++ "." ++ key)) val
                        |> Maybe.withDefault (matchBindings (name ++ "." ++ key) (Expr.Name key))
                )
                entries
                |> List.concat

        Expr.Spread _ ->
            [ Pretty.string "throw new Error(\"TODO: Implement spread pattern bindings.\")"
            ]

        Expr.TemplateDestructure segments ->
            case Expr.bound (Expr.TemplateDestructure segments) of
                [] ->
                    []

                bindings ->
                    [ Pretty.string "const [ , "
                        |> Pretty.a (Pretty.join (Pretty.string ", ") <| List.map Pretty.string bindings)
                        |> Pretty.a (Pretty.string " ] = new RegExp('^")
                        |> Pretty.a (Pretty.string <| String.join "" <| List.map (Data.Either.extract escapeRegex (Basics.always "(.*)")) segments)
                        |> Pretty.a (Pretty.string "$').exec(")
                        |> Pretty.a (Pretty.string name)
                        |> Pretty.a (Pretty.string ")")
                    ]

        Expr.Typeof _ p ->
            matchBindings name p

        Expr.VariantDestructure tagName ps ->
            matchBindings name <|
                Expr.ArrayDestructure (Expr.LiteralPattern (Expr.String <| "$" ++ tagName) :: ps)

        Expr.Wildcard _ ->
            []



-- UTILITIES -------------------------------------------------------------------


{-| -}
iife : ( String, Pretty.Doc t ) -> Pretty.Doc t -> Pretty.Doc t
iife ( arg, expr ) body =
    Pretty.string ("((" ++ arg ++ ") => ")
        |> Pretty.a body
        |> Pretty.a (Pretty.string ")")
        |> Pretty.a (Pretty.parens expr)


{-| -}
escapeRegex : String -> String
escapeRegex =
    let
        regexEscape =
            Regex.fromString "[.*+?^${}()|[\\]\\\\]" |> Maybe.withDefault Regex.never

        replaceFunc reMatch =
            "\\" ++ reMatch.match
    in
    Regex.replace regexEscape replaceFunc
