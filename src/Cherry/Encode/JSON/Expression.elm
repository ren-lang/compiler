module Cherry.Encode.JSON.Expression exposing (..)


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Expression exposing (..)
import Json.Encode


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
    Json.Encode.object
        [ ( "$", Json.Encode.string "Expression.Access.Computed" )
        , ( "expr", encoder expr )
        , ( "key", encoder key)
        ]

fixedAccessorEncoder : Expression -> String -> Json.Encode.Value
fixedAccessorEncoder expr key =
    Json.Encode.object
        [ ( "$", Json.Encode.string "Expression.Access.Fixed" )
        , ( "expr", encoder expr )
        , ( "key", Json.Encode.string key )
        ]


-- APPLICATION ENCODER ---------------------------------------------------------


applicationEncoder : Expression -> Expression -> Json.Encode.Value
applicationEncoder fn arg =
    Json.Encode.object
        [ ( "$", Json.Encode.string "Expression.Application" )
        , ( "fn", encoder fn )
        , ( "arg", encoder arg )
        ]


-- INFIXOP ENCODERS ------------------------------------------------------------


infixOpEncoder : Operator -> Expression -> Expression -> Json.Encode.Value
infixOpEncoder op lhs rhs =
    Json.Encode.object
        [ ( "$", Json.Encode.string "Expression.InfixOp" )
        , ( "op", operatorEncoder op )
        , ( "lhs", encoder lhs )
        , ( "rhs", encoder rhs )
        ]

operatorEncoder : Operator -> Json.Encode.Value
operatorEncoder operator =
    case operator of
        Pipe ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Pipe" ) 
                ]

        Compose ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Compose" ) 
                ]

        Discard ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Discard" ) 
                ]

        Add ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Add" ) 
                ]

        Sub ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Sub" ) 
                ]

        Mul ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Mul" ) 
                ]

        Div ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Div" ) 
                ]

        Pow ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Pow" ) 
                ]

        Mod ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Mod" ) 
                ]

        Eq ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Eq" ) 
                ]

        NotEq ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.NotEq" ) 
                ]

        Lt ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Lt" ) 
                ]

        Lte ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Lte" ) 
                ]

        Gt ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Gt" ) 
                ]

        Gte ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Gte" ) 
                ]

        And ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.And" ) 
                ]

        Or ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Or" ) 
                ]

        Cons ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Cons" ) 
                ]

        Join ->
            Json.Encode.object 
                [ ( "$", Json.Encode.string "Expression.Operator.Join" ) 
                ]


-- CONDITIONAL ENCODER ---------------------------------------------------------


conditionalEncoder : Expression -> Expression -> Expression -> Json.Encode.Value
conditionalEncoder predicate ifTrue ifFalse =
    Json.Encode.object
        [ ( "$", Json.Encode.string "Expression.Conditional" )
        , ( "if", encoder predicate )
        , ( "then", encoder ifTrue )
        , ( "else", encoder ifFalse )
        ]


-- LAMBDA ENCODER --------------------------------------------------------------


lambdaEncoder : List Variable -> Expression -> Json.Encode.Value
lambdaEncoder args body =
    Json.Encode.object
        [ ( "$", Json.Encode.string "Expression.Lambda" )
        , ( "args", Json.Encode.list variableEncoder)
        ]
