module Cherry.Encode.JSON.Expression exposing (..)


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Expression exposing (..)
import Json.Encode
import Json.Encode.Extra


-- RUNNING THE EMITTER ---------------------------------------------------------


encode : Expression -> String
encode expression =
    encoder expression
        |> Json.Encode.encode 4



-- ENCODERS --------------------------------------------------------------------


encoder : Expression -> Json.Encode.Value
encoder expression =
    case expression of
        Access expr accessor ->
            accessEncoder expr accessor

        Application fn arg ->
            applicationEncoder fn arg

        InfixOp op lhs rhs ->
            infixOpEncoder op lhs rhs

        Conditional predicate ifTrue ifFalse ->
            conditionalEncoder predicate ifTrue ifFalse

        Lambda args body ->
            lambdaEncoder args body

        Literal literal ->
            literalEncoder literal

        Variable variable ->
            variableEncoder variable


-- ACCESS ENCODERS -------------------------------------------------------------


accessEncoder : Expression -> Accessor -> Json.Encode.Value
accessEncoder expr accessor =
    case accessor of
        Computed key ->
            computedAccessorEncoder expr key

        Fixed key ->
            fixedAccessorEncoder expr key

computedAccessorEncoder : Expression -> Expression -> Json.Encode.Value
computedAccessorEncoder expr key =
    Json.Encode.Extra.taggedObject "Expression.Access.Computed"
        [ ( "expr", encoder expr )
        , ( "key", encoder key)
        ]

fixedAccessorEncoder : Expression -> String -> Json.Encode.Value
fixedAccessorEncoder expr key =
    Json.Encode.Extra.taggedObject "Expression.Access.Fixed"
        [ ( "expr", encoder expr )
        , ( "key", Json.Encode.string key )
        ]


-- APPLICATION ENCODER ---------------------------------------------------------


applicationEncoder : Expression -> Expression -> Json.Encode.Value
applicationEncoder fn arg =
    Json.Encode.Extra.taggedObject "Expression.Application"
        [ ( "fn", encoder fn )
        , ( "arg", encoder arg )
        ]


-- INFIXOP ENCODERS ------------------------------------------------------------


infixOpEncoder : Operator -> Expression -> Expression -> Json.Encode.Value
infixOpEncoder op lhs rhs =
    Json.Encode.Extra.taggedObject "Expression.InfixOp"
        [ ( "op", operatorEncoder op )
        , ( "lhs", encoder lhs )
        , ( "rhs", encoder rhs )
        ]

operatorEncoder : Operator -> Json.Encode.Value
operatorEncoder operator =
    case operator of
        Pipe ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Pipe" []

        Compose ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Compose" []

        Discard ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Discard" []

        Add ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Add" []

        Sub ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Sub" []

        Mul ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Mul" []

        Div ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Div" []

        Pow ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Pow" []

        Mod ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Mod" []

        Eq ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Eq" []

        NotEq ->
            Json.Encode.Extra.taggedObject "Expression.Operator.NotEq" []

        Lt ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Lt" []

        Lte ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Lte" []

        Gt ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Gt" []

        Gte ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Gte" []

        And ->
            Json.Encode.Extra.taggedObject "Expression.Operator.And" []

        Or ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Or" []

        Cons ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Cons" []

        Join ->
            Json.Encode.Extra.taggedObject "Expression.Operator.Join" []


-- CONDITIONAL ENCODER ---------------------------------------------------------


conditionalEncoder : Expression -> Expression -> Expression -> Json.Encode.Value
conditionalEncoder predicate ifTrue ifFalse =
    Json.Encode.Extra.taggedObject "Expression.Conditional"
        [ ( "if", encoder predicate )
        , ( "then", encoder ifTrue )
        , ( "else", encoder ifFalse )
        ]


-- LAMBDA ENCODER --------------------------------------------------------------


lambdaEncoder : List Variable -> Expression -> Json.Encode.Value
lambdaEncoder args body =
    Json.Encode.Extra.taggedObject "Expression.Lambda"
        [ ( "args", Json.Encode.list variableEncoder args)
        , ( "body", encoder body )
        ]


-- LITERAL ENCODERS ------------------------------------------------------------


literalEncoder : Literal -> Json.Encode.Value
literalEncoder literal =
    case literal of
        Array elements ->
            literalArrayEncoder elements

        Boolean b ->
            literalBooleanEncoder b

        Number n ->
            literalNumberEncoder n

        Object entries ->
            literalObjectEncoder entries

        String s ->
            literalStringEncoder s

literalArrayEncoder : List (Expression) -> Json.Encode.Value
literalArrayEncoder elements =
    Json.Encode.Extra.taggedObject "Expression.Literal.Array"
        [ ( "elements", Json.Encode.list encoder elements )
        ]

literalBooleanEncoder : Bool -> Json.Encode.Value
literalBooleanEncoder b =
    Json.Encode.Extra.taggedObject "Expression.Literal.Boolean"
        [ ( "b", Json.Encode.bool b )
        ]

literalNumberEncoder : Float -> Json.Encode.Value
literalNumberEncoder n =
    Json.Encode.Extra.taggedObject "Expression.Literal.Number"
        [ ( "n", Json.Encode.float n )
        ]

literalObjectEncoder : List ( String, Expression ) -> Json.Encode.Value
literalObjectEncoder entries =
    let
        literalObjectFieldEncoder ( k, v ) =
            Json.Encode.Extra.taggedObject "Expression.Literal.Object.Field"
                [ ( "key", Json.Encode.string k )
                , ( "value", encoder v )
                ]
    in
    Json.Encode.Extra.taggedObject "Expression.Literal.Object"
        [ ( "entries", Json.Encode.list literalObjectFieldEncoder entries)
        ]

literalStringEncoder : String -> Json.Encode.Value
literalStringEncoder s =
    Json.Encode.Extra.taggedObject "Expression.Literal.String"
        [ ( "s", Json.Encode.string s )
        ]


-- VARIABLE ENCODERS -----------------------------------------------------------


variableEncoder : Variable -> Json.Encode.Value
variableEncoder variable =
    case variable of
        ArrayDestructure bindings ->
            variableArrayDestructureEncoder bindings

        Local name ->
            variableLocalEncoder name

        ObjectDestructure bindings ->
            variableObjectDestructureEncoder bindings

        Operator op ->
            variableOperatorEncoder op

        Scoped scopes name ->
            variableScopedEncoder scopes name

variableArrayDestructureEncoder : List (Variable) -> Json.Encode.Value
variableArrayDestructureEncoder bindings =
    Json.Encode.Extra.taggedObject "Expression.Variable.ArrayDestructure"
        [ ("bindings", Json.Encode.list variableEncoder bindings )
        ]

variableLocalEncoder : String -> Json.Encode.Value
variableLocalEncoder name =
    Json.Encode.Extra.taggedObject "Expression.Variable.Local"
        [ ( "name", Json.Encode.string name )
        ]

variableObjectDestructureEncoder : List Variable -> Json.Encode.Value
variableObjectDestructureEncoder bindings =
    Json.Encode.Extra.taggedObject "Expression.Variable.ObjectDestructure"
        [ ( "bindings", Json.Encode.list variableEncoder bindings )
        ]

variableOperatorEncoder : Operator -> Json.Encode.Value
variableOperatorEncoder op =
    Json.Encode.Extra.taggedObject "Expression.Variable.Operator"
        [ ( "op", operatorEncoder op )
        ]

variableScopedEncoder : List String -> String -> Json.Encode.Value
variableScopedEncoder scopes name =
    Json.Encode.Extra.taggedObject "Expression.Variable.Scoped"
        [ ( "scopes", Json.Encode.list Json.Encode.string scopes )
        , ( "name", Json.Encode.string name )
        ]
