module Cherry.Stage.Generate.JSON.Expression.Identifier exposing 
    ( generator
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Generate.JSON.Expression.Operator as Operator
import Json.Encode
import Json.Encode.Extra


-- GENERATORS ------------------------------------------------------------------


{-| -}
generator : AST.Identifier -> Json.Encode.Value
generator identifier =
    case identifier of
        AST.Local name ->
            localGenerator name

        AST.Scoped namespace name ->
            scopedGenerator namespace name

        AST.Operator op ->
            operatorGenerator op

{-| -}
localGenerator : String -> Json.Encode.Value
localGenerator name =
    Json.Encode.Extra.taggedObject "AST.Identifier.Local"
        [ ( "name", Json.Encode.string name )
        ]

{-| -}
scopedGenerator : List String -> String -> Json.Encode.Value
scopedGenerator namespace name =
    Json.Encode.Extra.taggedObject "AST.Identifier.Scoped"
        [ ( "namespace", Json.Encode.list Json.Encode.string namespace )
        , ( "name", Json.Encode.string name )
        ]

{-| -}
operatorGenerator : AST.Operator -> Json.Encode.Value
operatorGenerator op =
    Json.Encode.Extra.taggedObject "AST.Identifier.Operator"
        [ ( "op", Operator.generator op )
        ]

