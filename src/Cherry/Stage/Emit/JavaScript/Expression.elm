module Cherry.Stage.Emit.JavaScript.Expression exposing 
    ( emit 
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JavaScript.Expression.Identifier as Identifier
import Cherry.Stage.Emit.JavaScript.Expression.Literal as Literal
import Cherry.Stage.Emit.JavaScript.Expression.Pattern as Pattern
import Cherry.Stage.Emit.JavaScript.Expression.Identifier exposing (emit)


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit : AST.Expression -> String
emit expression =
    case expression of
        AST.Access expr accessors ->
            accessEmitter expr accessors

        AST.Application func args ->
            applicationEmitter func args

        AST.Identifier identifier ->
            Identifier.emit identifier

        AST.Infix op lhs rhs ->
            infixEmitter op lhs rhs

        AST.Conditional predicate true false ->
            conditionalEmitter predicate true false

        AST.Lambda args body ->
            lambdaEmitter args body

        AST.Literal literal ->
            Literal.emit emit literal


-- ACCESS EMITTERS -----------------------------------------------------------


{-| -}
accessEmitter : AST.Expression -> List AST.Accessor -> String
accessEmitter expr accessors =
    "{expr}{accessors}"
        |> String.replace "{expr}" (emit expr)
        |> String.replace "{accessors}" (List.map accessorEmitter accessors |> String.join "")

{-| -}
accessorEmitter : AST.Accessor -> String
accessorEmitter accessor =
    case accessor of
        AST.Computed expr ->
            "[{expr}]"
                |> String.replace "{expr}" (emit expr)
        
        AST.Fixed key ->
            ".{key}" 
                |> String.replace "{key}" key


-- APPLICATION EMITTER -------------------------------------------------------


{-| -}
applicationEmitter : AST.Expression -> List AST.Expression -> String
applicationEmitter func args =
    "{func} {args}"
        |> String.replace "{func}" (emit func)
        |> String.replace "{args}" (applicationArgsEmitter args)

{-| -}
applicationArgsEmitter : List AST.Expression  -> String
applicationArgsEmitter args =
    List.map (\arg -> "(" ++ emit arg ++ ")") args
        |> String.join " "


-- INFIX EMITTER -------------------------------------------------------------


{-| -}
infixEmitter : AST.Operator -> AST.Expression -> AST.Expression -> String
infixEmitter op lhs rhs =
    case op of
        AST.Pipe ->
            applicationEmitter rhs [ lhs ]

        AST.Compose ->
            applicationEmitter 
                (AST.Identifier (AST.Scoped [ "$Function" ] "compose"))
                [ lhs, rhs ]

        AST.Discard ->
            "{lhs}, {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)
    
        AST.Add ->
            "{lhs} + {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Sub ->
            "{lhs} - {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Mul ->
            "{lhs} * {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Div ->
            "{lhs} / {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Pow ->
            "{lhs} ** {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Mod ->
            "{lhs} % {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Eq ->
            "{lhs} == {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.NotEq ->
            "{lhs} != {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Lt ->
            "{lhs} < {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Lte ->
            "{lhs} <= {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Gt ->
            "{lhs} > {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Gte ->
            "{lhs} >= {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.And ->
            "{lhs} && {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Or ->
            "{lhs} || {rhs}"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Cons ->
            "[ {lhs}, ...{rhs} ]"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)

        AST.Join ->
            "[ ...{lhs}, ...{rhs} ]"
                |> String.replace "{lhs}" (emit lhs)
                |> String.replace "{rhs}" (emit rhs)


-- CONDITIONAL EMITTER -------------------------------------------------------


{-| -}
conditionalEmitter : AST.Expression -> AST.Expression -> AST.Expression -> String
conditionalEmitter predicate true false =
    "{predicate} ? {true} : {false}"
        |> String.replace "{predicate}" (emit predicate)
        |> String.replace "{true}" (emit true)
        |> String.replace "{false}" (emit false)


-- LAMBDA EMITTER ------------------------------------------------------------


{-| -}
lambdaEmitter : List AST.Pattern -> AST.Expression -> String
lambdaEmitter args body =
    "{args} => {body}"
        |> String.replace "{args}" (lambdaArgsEmitter args)
        |> String.replace "{body}" (emit body)

{-| -}
lambdaArgsEmitter : List AST.Pattern -> String
lambdaArgsEmitter args =
    List.map (\arg -> "(" ++ Pattern.emit emit arg ++ ")") args
        |> String.join " => "