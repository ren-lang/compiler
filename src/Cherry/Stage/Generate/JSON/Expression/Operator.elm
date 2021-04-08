module Cherry.Stage.Generate.JSON.Expression.Operator exposing
    ( generator
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Json.Encode
import Json.Encode.Extra


-- GENERATORS ------------------------------------------------------------------


{-| -}
generator : AST.Operator -> Json.Encode.Value
generator operator =
    case operator of
        AST.Pipe ->
            Json.Encode.Extra.taggedObject "AST.Operator.Pipe" []

        AST.Compose ->
            Json.Encode.Extra.taggedObject "AST.Operator.Compose" []

        AST.Discard ->
            Json.Encode.Extra.taggedObject "AST.Operator.Discard" []

        AST.Add ->
            Json.Encode.Extra.taggedObject "AST.Operator.Add" []

        AST.Sub ->
            Json.Encode.Extra.taggedObject "AST.Operator.Sub" []

        AST.Mul ->
            Json.Encode.Extra.taggedObject "AST.Operator.Mul" []

        AST.Div ->
            Json.Encode.Extra.taggedObject "AST.Operator.Div" []

        AST.Pow ->
            Json.Encode.Extra.taggedObject "AST.Operator.Pow" []

        AST.Mod ->
            Json.Encode.Extra.taggedObject "AST.Operator.Mod" []

        AST.Eq ->
            Json.Encode.Extra.taggedObject "AST.Operator.Eq" []

        AST.NotEq ->
            Json.Encode.Extra.taggedObject "AST.Operator.NotEq" []

        AST.Lt ->
            Json.Encode.Extra.taggedObject "AST.Operator.Lt" []

        AST.Lte ->
            Json.Encode.Extra.taggedObject "AST.Operator.Lte" []

        AST.Gt ->
            Json.Encode.Extra.taggedObject "AST.Operator.Gt" []

        AST.Gte ->
            Json.Encode.Extra.taggedObject "AST.Operator.Gte" []

        AST.And ->
            Json.Encode.Extra.taggedObject "AST.Operator.And" []

        AST.Or ->
            Json.Encode.Extra.taggedObject "AST.Operator.Or" []

        AST.Cons ->
            Json.Encode.Extra.taggedObject "AST.Operator.Cons" []

        AST.Join ->
            Json.Encode.Extra.taggedObject "AST.Operator.Join" []
