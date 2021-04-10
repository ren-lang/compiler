module Cherry.Stage.Emit.JSON.Declaration exposing
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JSON.Expression as Expression
import Cherry.Stage.Emit.JSON.Expression.Pattern as Pattern
import Json.Encode
import Json.Encode.Extra


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit : AST.Declaration -> Json.Encode.Value
emit declaration =
    case declaration of
        AST.Fun visibility name args body bindings ->
            funEmitter visibility name args body bindings

        AST.Let visibility name body bindings ->
            letEmitter visibility name body bindings

{-| -}
funEmitter : AST.Visibility -> String -> List AST.Pattern -> AST.Expression -> List AST.Binding -> Json.Encode.Value
funEmitter visibility name args body bindings =
    Json.Encode.Extra.taggedObject "AST.Declaration.Fun"
        [ ( "visibility", visibilityEmitter visibility )
        , ( "name", Json.Encode.string name )
        , ( "args", Json.Encode.list (Pattern.emit Expression.emit) args )
        , ( "body", Expression.emit body )
        , ( "bindings", Json.Encode.list bindingEmitter bindings )
        ]

{-| -}
letEmitter : AST.Visibility -> String -> AST.Expression -> List AST.Binding -> Json.Encode.Value
letEmitter visibility name body bindings =
    Json.Encode.Extra.taggedObject "AST.Declaration.Let"
        [ ( "visibility", visibilityEmitter visibility )
        , ( "name", Json.Encode.string name )
        , ( "body", Expression.emit body )
        , ( "bindings", Json.Encode.list bindingEmitter bindings )
        ]


-- VISIBILITY EMITTER --------------------------------------------------------


{-| -}
visibilityEmitter : AST.Visibility -> Json.Encode.Value
visibilityEmitter visibility =
    case visibility of
        AST.Public ->
            Json.Encode.Extra.taggedObject "AST.Visibility.Public" []

        AST.Private ->
            Json.Encode.Extra.taggedObject "AST.Visibility.Private" []


-- BINDING EMITTER -----------------------------------------------------------


{-| -}
bindingEmitter : AST.Binding -> Json.Encode.Value
bindingEmitter { name, body } =
    Json.Encode.Extra.taggedObject "AST.Binding"
        [ ( "name", Json.Encode.string name )
        , ( "body", Expression.emit body )
        ]
