module Cherry.Stage.Emit.JSON.Expression.Literal exposing 
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Json.Encode
import Json.Encode.Extra


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit: (AST.Expression -> Json.Encode.Value) -> AST.Literal -> Json.Encode.Value
emit emitExpression literal =
    case literal of
        AST.Array elements ->
            arrayEmitter emitExpression elements

        AST.Boolean b ->
            booleanEmitter b

        AST.Number n ->
            numberEmitter n

        AST.Object entries ->
            objectEmitter emitExpression entries

        AST.String s ->
            stringEmitter s

{-| -}
arrayEmitter : (AST.Expression -> Json.Encode.Value) -> List AST.Expression -> Json.Encode.Value
arrayEmitter emitExpression elements =
    Json.Encode.Extra.taggedObject "AST.Literal.Array"
        [ ( "elements", Json.Encode.list emitExpression elements )
        ]

{-| -}
booleanEmitter : Bool -> Json.Encode.Value
booleanEmitter b =
    Json.Encode.Extra.taggedObject "AST.Literal.Boolean"
        [ ("b", Json.Encode.bool b )
        ]

{-| -}
numberEmitter : Float -> Json.Encode.Value
numberEmitter n =
    Json.Encode.Extra.taggedObject "AST.Literal.Number"
        [ ( "n", Json.Encode.float n )
        ]

{-| -}
objectEmitter : (AST.Expression -> Json.Encode.Value) -> List ( String, AST.Expression ) -> Json.Encode.Value
objectEmitter emitExpression entries =
    Json.Encode.Extra.taggedObject "AST.Literal.Object"
        [ ( "entires", Json.Encode.list (objectEntryEmitter emitExpression) entries )
        ]

{-| -}
objectEntryEmitter : (AST.Expression -> Json.Encode.Value) -> ( String, AST.Expression ) -> Json.Encode.Value
objectEntryEmitter emitExpression ( key, val ) =
    Json.Encode.Extra.taggedObject "AST.Literal.Object.Entry"
        [ ( "key", Json.Encode.string key )
        , ( "val", emitExpression val )
        ]

{-| -}
stringEmitter : String -> Json.Encode.Value
stringEmitter s =
    Json.Encode.Extra.taggedObject "AST.Literal.String"
        [ ( "s", Json.Encode.string s )
        ]
