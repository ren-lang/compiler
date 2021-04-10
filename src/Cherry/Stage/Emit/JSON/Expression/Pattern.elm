module Cherry.Stage.Emit.JSON.Expression.Pattern exposing
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JSON.Expression.Literal as Literal
import Json.Encode
import Json.Encode.Extra


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit: (AST.Expression -> Json.Encode.Value) -> AST.Pattern -> Json.Encode.Value
emit emitExpression pattern =
    case pattern of
        AST.ArrayDestructure patterns ->
            arrayDestructureEmitter emitExpression patterns

        AST.Name name ->
            nameEmitter name

        AST.ObjectDestructure patterns ->
            objectDestructureEmitter emitExpression patterns

        AST.Value literal ->
            valueEmitter emitExpression literal

{-| -}
arrayDestructureEmitter : (AST.Expression -> Json.Encode.Value) -> List AST.Pattern -> Json.Encode.Value
arrayDestructureEmitter emitExpression patterns =
    Json.Encode.Extra.taggedObject "AST.Pattern.ArrayDestructure"
        [ ( "patterns", Json.Encode.list (emit emitExpression) patterns )
        ]

{-| -}
nameEmitter : String -> Json.Encode.Value
nameEmitter name =
    Json.Encode.Extra.taggedObject "AST.Pattern.Name"
        [ ( "name", Json.Encode.string name )
        ]

{-| -}
objectDestructureEmitter : (AST.Expression -> Json.Encode.Value) -> List ( String, Maybe AST.Pattern ) -> Json.Encode.Value
objectDestructureEmitter emitExpression patterns =
    Json.Encode.Extra.taggedObject "AST.Pattern.ObjectDestructure"
        [ ( "patterns", Json.Encode.list (objectDestructurePatternEmitter emitExpression) patterns )
        ]

{-| -}
objectDestructurePatternEmitter : (AST.Expression -> Json.Encode.Value) -> ( String, Maybe AST.Pattern ) -> Json.Encode.Value
objectDestructurePatternEmitter emitExpression pattern =
    case pattern of
        ( key, Just nestedPattern ) ->
            Json.Encode.Extra.taggedObject "AST.Pattern.ObjectDestructure.Nested"
                [ ( "key", Json.Encode.string key )
                , ( "nestedPattern", emit emitExpression nestedPattern )
                ]
            
        ( key, Nothing ) ->
            Json.Encode.Extra.taggedObject "AST.Pattern.ObjectDestructure.Simple"
                [ ( "key", Json.Encode.string key )
                ]

{-| -}
valueEmitter : (AST.Expression -> Json.Encode.Value) -> AST.Literal -> Json.Encode.Value
valueEmitter emitExpression literal =
    Json.Encode.Extra.taggedObject "AST.Pattern.Value"
        [ ( "literal", Literal.emit emitExpression literal )
        ]

