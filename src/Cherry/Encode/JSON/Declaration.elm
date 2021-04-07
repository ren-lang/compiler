module Cherry.Encode.JSON.Declaration exposing (..)


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Declaration exposing (..)
import Cherry.AST.Expression as Expression exposing (Expression)
import Cherry.Encode.JSON.Expression as Expression
import Json.Encode
import Json.Encode.Extra


-- RUNNING THE ENCODER ---------------------------------------------------------


{-| -}
encode : Declaration -> String
encode declaration =
    encoder declaration
        |> Json.Encode.encode 4


-- ENCODERS --------------------------------------------------------------------


{-| -}
encoder : Declaration -> Json.Encode.Value
encoder declaration =
    case declaration of
        Function isPublic name args body bindings ->
            functionEncoder isPublic name args body bindings

        Variable isPublic name body bindings ->
            variableEncoder isPublic name body bindings


-- FUNCTION ENCODER ------------------------------------------------------------


{-| -}
functionEncoder : Bool -> String -> List Expression.Variable -> Expression -> List ( String, Expression ) -> Json.Encode.Value
functionEncoder isPublic name args body bindings =
    Json.Encode.Extra.taggedObject "Declaration.Function"
        [ ( "isPublic", Json.Encode.bool isPublic )
        , ( "name", Json.Encode.string name )
        , ( "args", Json.Encode.list Expression.variableEncoder args)
        , ( "body", Expression.encoder body )
        , ( "bindings", Json.Encode.list bindingEncoder bindings )
        ]


-- VARIABLE ENCODER ------------------------------------------------------------


{-| -}
variableEncoder : Bool -> String -> Expression -> List ( String, Expression ) -> Json.Encode.Value
variableEncoder isPublic name body bindings =
    Json.Encode.Extra.taggedObject "Declaration.Variable"
        [ ( "isPublic", Json.Encode.bool isPublic )
        , ( "name", Json.Encode.string name )
        , ( "body", Expression.encoder body )
        , ( "bindings", Json.Encode.list bindingEncoder bindings )
        ]


-- BINDING ENCODER -------------------------------------------------------------


{-| -}
bindingEncoder : ( String, Expression ) -> Json.Encode.Value
bindingEncoder ( name, body ) =
    Json.Encode.Extra.taggedObject "Declaration.Binding"
        [ ( "name", Json.Encode.string name )
        , ( "body", Expression.encoder body )
        ]