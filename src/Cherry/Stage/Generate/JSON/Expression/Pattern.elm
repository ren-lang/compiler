module Cherry.Stage.Generate.JSON.Expression.Pattern exposing
    ( generator
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Generate.JSON.Expression.Literal as Literal
import Json.Encode
import Json.Encode.Extra


-- GENERATORS ------------------------------------------------------------------


{-| -}
generator : (AST.Expression -> Json.Encode.Value) -> AST.Pattern -> Json.Encode.Value
generator expressionGenerator pattern =
    case pattern of
        AST.ArrayDestructure patterns ->
            arrayDestructureGenerator expressionGenerator patterns

        AST.Name name ->
            nameGenerator name

        AST.ObjectDestructure patterns ->
            objectDestructureGenerator expressionGenerator patterns

        AST.Value literal ->
            valueGenerator expressionGenerator literal

{-| -}
arrayDestructureGenerator : (AST.Expression -> Json.Encode.Value) -> List AST.Pattern -> Json.Encode.Value
arrayDestructureGenerator expressionGenerator patterns =
    Json.Encode.Extra.taggedObject "AST.Pattern.ArrayDestructure"
        [ ( "patterns", Json.Encode.list (generator expressionGenerator) patterns )
        ]

{-| -}
nameGenerator : String -> Json.Encode.Value
nameGenerator name =
    Json.Encode.Extra.taggedObject "AST.Pattern.Name"
        [ ( "name", Json.Encode.string name )
        ]

{-| -}
objectDestructureGenerator : (AST.Expression -> Json.Encode.Value) -> List ( String, Maybe AST.Pattern ) -> Json.Encode.Value
objectDestructureGenerator expressionGenerator patterns =
    Json.Encode.Extra.taggedObject "AST.Pattern.ObjectDestructure"
        [ ( "patterns", Json.Encode.list (objectDestructurePatternGenerator expressionGenerator) patterns )
        ]

{-| -}
objectDestructurePatternGenerator : (AST.Expression -> Json.Encode.Value) -> ( String, Maybe AST.Pattern ) -> Json.Encode.Value
objectDestructurePatternGenerator expressionGenerator pattern =
    case pattern of
        ( key, Just nestedPattern ) ->
            Json.Encode.Extra.taggedObject "AST.Pattern.ObjectDestructure.Nested"
                [ ( "key", Json.Encode.string key )
                , ( "nestedPattern", generator expressionGenerator nestedPattern )
                ]
            
        ( key, Nothing ) ->
            Json.Encode.Extra.taggedObject "AST.Pattern.ObjectDestructure.Simple"
                [ ( "key", Json.Encode.string key )
                ]

{-| -}
valueGenerator : (AST.Expression -> Json.Encode.Value) -> AST.Literal -> Json.Encode.Value
valueGenerator expressionGenerator literal =
    Json.Encode.Extra.taggedObject "AST.Pattern.Value"
        [ ( "literal", Literal.generator expressionGenerator literal )
        ]

