module Cherry.Stage.Emit.JavaScript.Expression.Pattern exposing
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JavaScript.Expression.Literal as Literal


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit: (AST.Expression -> String) -> AST.Pattern -> String
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
arrayDestructureEmitter : (AST.Expression -> String) -> List AST.Pattern -> String
arrayDestructureEmitter emitExpression patterns =
    "[ {patterns} ]"
        |> String.replace "{patterns}" (List.map (emit emitExpression) patterns |> String.join ", ")

{-| -}
nameEmitter : String -> String
nameEmitter name =
    name

{-| -}
objectDestructureEmitter : (AST.Expression -> String) -> List ( String, Maybe AST.Pattern ) -> String
objectDestructureEmitter emitExpression patterns =
    "{ {patterns} }"
        |> String.replace "{patterns}" (List.map (objectDestructurePatternEmitter emitExpression) patterns |> String.join ", ")

{-| -}
objectDestructurePatternEmitter : (AST.Expression -> String) -> ( String, Maybe AST.Pattern ) -> String
objectDestructurePatternEmitter emitExpression pattern =
    case pattern of
        ( key, Just nestedPattern ) ->
            "{key}: {pattern}"
                |> String.replace "{key}" key
                |> String.replace "{pattern}" (emit emitExpression nestedPattern)
            
        ( key, Nothing ) ->
            key

{-| -}
valueEmitter : (AST.Expression -> String) -> AST.Literal -> String
valueEmitter emitExpression literal =
    Literal.emit emitExpression literal

