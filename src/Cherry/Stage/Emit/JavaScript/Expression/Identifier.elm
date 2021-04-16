module Cherry.Stage.Emit.JavaScript.Expression.Identifier exposing 
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit: AST.Identifier -> String
emit identifier =
    case identifier of
        AST.Local name ->
            localEmitter name

        AST.Scoped namespace name ->
            scopedEmitter namespace name

        AST.Operator op ->
            operatorEmitter op

        AST.ObjectField fieldName ->
            objectFieldEmitter fieldName

{-| -}
localEmitter : String -> String
localEmitter name =
    name

{-| -}
scopedEmitter : List String -> String -> String
scopedEmitter namespace name =
    "{namespace}.{name}"
        |> String.replace "{namespace}" (String.join "$" namespace)
        |> String.replace "{name}" name

{-| -}
operatorEmitter : AST.Operator -> String
operatorEmitter op =
    case op of
        AST.Pipe ->
            "$Function.pipe" 

        AST.Compose ->
            "$Function.compose"

        AST.Discard ->
            "$Function.discard"
    
        AST.Add ->
            "$Math.add"

        AST.Sub ->
            "$Math.sub"

        AST.Mul ->
            "$Math.mul"

        AST.Div ->
            "$Math.div"

        AST.Pow ->
            "$Math.pow"

        AST.Mod ->
            "$Math.mod"

        AST.Eq ->
            "$Compare.eq"

        AST.NotEq ->
            "$Compare.notEq"

        AST.Lt ->
            "$Compare.lt"

        AST.Lte ->
            "$Compare.lte"

        AST.Gt ->
            "$Compare.gt"

        AST.Gte ->
            "$Compare.gte"

        AST.And ->
            "$Logic.and"

        AST.Or ->
            "$Logic.or"

        AST.Cons ->
            "$Array.cons"

        AST.Join ->
            "$Array.join"

{-| -}
objectFieldEmitter : String -> String
objectFieldEmitter fieldName =
    "$Object.get {fieldName}"
        |> String.replace "{fieldName}" fieldName
