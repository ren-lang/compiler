module Cherry.Stage.Emit.JavaScript.Expression.Literal exposing 
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit: (AST.Expression -> String) -> AST.Literal -> String
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
arrayEmitter : (AST.Expression -> String) -> List AST.Expression -> String
arrayEmitter emitExpression elements =
    "[ {elements} ]"
        |> String.replace "{elements}" (List.map emitExpression elements |> String.join ", ")

{-| -}
booleanEmitter : Bool -> String
booleanEmitter b =
    if b then
        "true"

    else
        "false"

{-| -}
numberEmitter : Float -> String
numberEmitter n =
    String.fromFloat n

{-| -}
objectEmitter : (AST.Expression -> String) -> List ( String, AST.Expression ) -> String
objectEmitter emitExpression entries =
    "{ {entries} }"
        |> String.replace "{entries}" (List.map (objectEntryEmitter emitExpression) entries |> String.join ", ")

{-| -}
objectEntryEmitter : (AST.Expression -> String) -> ( String, AST.Expression ) -> String
objectEntryEmitter emitExpression ( key, val ) =
    "{key}: {val}"
        |> String.replace "{key}" key
        |> String.replace "{val}" (emitExpression val)

{-| -}
stringEmitter : String -> String
stringEmitter s =
    "'{s}'"
        |> String.replace "{s}" s
