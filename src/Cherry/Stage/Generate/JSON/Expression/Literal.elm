module Cherry.Stage.Generate.JSON.Expression.Literal exposing 
    ( generator
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Json.Encode
import Json.Encode.Extra


-- GENERATORS ------------------------------------------------------------------


{-| -}
generator : (AST.Expression -> Json.Encode.Value) -> AST.Literal -> Json.Encode.Value
generator expressionGenerator literal =
    case literal of
        AST.Array elements ->
            arrayGenerator expressionGenerator elements

        AST.Boolean b ->
            booleanGenerator b

        AST.Number n ->
            numberGenerator n

        AST.Object entries ->
            objectGenerator expressionGenerator entries

        AST.String s ->
            stringGenerator s

{-| -}
arrayGenerator : (AST.Expression -> Json.Encode.Value) -> List AST.Expression -> Json.Encode.Value
arrayGenerator expressionGenerator elements =
    Json.Encode.Extra.taggedObject "AST.Literal.Array"
        [ ( "elements", Json.Encode.list expressionGenerator elements )
        ]

{-| -}
booleanGenerator : Bool -> Json.Encode.Value
booleanGenerator b =
    Json.Encode.Extra.taggedObject "AST.Literal.Boolean"
        [ ("b", Json.Encode.bool b )
        ]

{-| -}
numberGenerator : Float -> Json.Encode.Value
numberGenerator n =
    Json.Encode.Extra.taggedObject "AST.Literal.Number"
        [ ( "n", Json.Encode.float n )
        ]

{-| -}
objectGenerator : (AST.Expression -> Json.Encode.Value) -> List ( String, AST.Expression ) -> Json.Encode.Value
objectGenerator expressionGenerator entries =
    Json.Encode.Extra.taggedObject "AST.Literal.Object"
        [ ( "entires", Json.Encode.list (objectEntryGenerator expressionGenerator) entries )
        ]

{-| -}
objectEntryGenerator : (AST.Expression -> Json.Encode.Value) -> ( String, AST.Expression ) -> Json.Encode.Value
objectEntryGenerator expressionGenerator ( key, val ) =
    Json.Encode.Extra.taggedObject "AST.Literal.Object.Entry"
        [ ( "key", Json.Encode.string key )
        , ( "val", expressionGenerator val )
        ]

{-| -}
stringGenerator : String -> Json.Encode.Value
stringGenerator s =
    Json.Encode.Extra.taggedObject "AST.Literal.String"
        [ ( "s", Json.Encode.string s )
        ]
