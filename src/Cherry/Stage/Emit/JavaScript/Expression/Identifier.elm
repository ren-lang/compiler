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
            "($x) => ($f) => $f($x)" 

        AST.Compose ->
            "($f) => ($g) => ($x) => $g($f($x))"

        AST.Discard ->
            "($x) => ($y) => $y"
    
        AST.Add ->
            "($x) => ($y) => $x + $y"

        AST.Sub ->
            "($x) => ($y) => $x - $y"

        AST.Mul ->
            "($x) => ($y) => $x * $y"

        AST.Div ->
            "($x) => ($y) => $x / $y"

        AST.Pow ->
            "($x) => ($y) => $x ** $y"

        AST.Mod ->
            "($x) => ($y) => $x % $y"

        AST.Eq ->
            "($x) => ($y) => $x == $y"

        AST.NotEq ->
            "($x) => ($y) => $x != $y"

        AST.Lt ->
            "($x) => ($y) => $x < $y"

        AST.Lte ->
            "($x) => ($y) => $x <= $y"

        AST.Gt ->
            "($x) => ($y) => $x > $y"

        AST.Gte ->
            "($x) => ($y) => $x >= $y"

        AST.And ->
            "($x) => ($y) => $x && $y"

        AST.Or ->
            "($x) => ($y) => $x || $y"

        AST.Cons ->
            "($x) => ($y) => ([ $x, ...$y ])"

        AST.Join ->
            "($x) => ($y) => ([ ...$x, ...$y ])"

{-| -}
objectFieldEmitter : String -> String
objectFieldEmitter fieldName =
    "($x) => $x.{fieldName}"
        |> String.replace "{fieldName}" fieldName
