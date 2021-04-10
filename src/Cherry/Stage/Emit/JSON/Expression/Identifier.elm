module Cherry.Stage.Emit.JSON.Expression.Identifier exposing 
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JSON.Expression.Operator as Operator
import Json.Encode
import Json.Encode.Extra


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit: AST.Identifier -> Json.Encode.Value
emit identifier =
    case identifier of
        AST.Local name ->
            localEmitter name

        AST.Scoped namespace name ->
            scopedEmitter namespace name

        AST.Operator op ->
            operatorEmitter op

{-| -}
localEmitter : String -> Json.Encode.Value
localEmitter name =
    Json.Encode.Extra.taggedObject "AST.Identifier.Local"
        [ ( "name", Json.Encode.string name )
        ]

{-| -}
scopedEmitter : List String -> String -> Json.Encode.Value
scopedEmitter namespace name =
    Json.Encode.Extra.taggedObject "AST.Identifier.Scoped"
        [ ( "namespace", Json.Encode.list Json.Encode.string namespace )
        , ( "name", Json.Encode.string name )
        ]

{-| -}
operatorEmitter : AST.Operator -> Json.Encode.Value
operatorEmitter op =
    Json.Encode.Extra.taggedObject "AST.Identifier.Operator"
        [ ( "op", Operator.emit op )
        ]

