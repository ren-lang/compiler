module Cherry.Stage.Generate.JSON.Declaration exposing
    ( run, generator
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Generate.JSON.Expression as Expression
import Cherry.Stage.Generate.JSON.Expression.Pattern as Pattern
import Json.Encode
import Json.Encode.Extra


-- RUNNING THE GENERATOR -------------------------------------------------------


{-| -}
run : AST.Declaration -> String
run ast =
    generator ast
        |> Json.Encode.encode 4


-- GENERATORS ------------------------------------------------------------------


{-| -}
generator : AST.Declaration -> Json.Encode.Value
generator declaration =
    case declaration of
        AST.Fun visibility name args body bindings ->
            funGenerator visibility name args body bindings

        AST.Let visibility name body bindings ->
            letGenerator visibility name body bindings

{-| -}
funGenerator : AST.Visibility -> String -> List AST.Pattern -> AST.Expression -> List AST.Binding -> Json.Encode.Value
funGenerator visibility name args body bindings =
    Json.Encode.Extra.taggedObject "AST.Declaration.Fun"
        [ ( "visibility", visibilityGenerator visibility )
        , ( "name", Json.Encode.string name )
        , ( "args", Json.Encode.list (Pattern.generator Expression.generator) args )
        , ( "body", Expression.generator body )
        , ( "bindings", Json.Encode.list bindingGenerator bindings )
        ]

{-| -}
letGenerator : AST.Visibility -> String -> AST.Expression -> List AST.Binding -> Json.Encode.Value
letGenerator visibility name body bindings =
    Json.Encode.Extra.taggedObject "AST.Declaration.Let"
        [ ( "visibility", visibilityGenerator visibility )
        , ( "name", Json.Encode.string name )
        , ( "body", Expression.generator body )
        , ( "bindings", Json.Encode.list bindingGenerator bindings )
        ]


-- VISIBILITY GENERATOR --------------------------------------------------------


{-| -}
visibilityGenerator : AST.Visibility -> Json.Encode.Value
visibilityGenerator visibility =
    case visibility of
        AST.Public ->
            Json.Encode.Extra.taggedObject "AST.Visibility.Public" []

        AST.Private ->
            Json.Encode.Extra.taggedObject "AST.Visibility.Private" []


-- BINDING GENERATOR -----------------------------------------------------------


{-| -}
bindingGenerator : AST.Binding -> Json.Encode.Value
bindingGenerator { name, body } =
    Json.Encode.Extra.taggedObject "AST.Binding"
        [ ( "name", Json.Encode.string name )
        , ( "body", Expression.generator body )
        ]
