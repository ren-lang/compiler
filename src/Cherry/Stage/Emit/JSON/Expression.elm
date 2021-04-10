module Cherry.Stage.Emit.JSON.Expression exposing 
    ( emit 
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JSON.Expression.Identifier as Identifier
import Cherry.Stage.Emit.JSON.Expression.Literal as Literal
import Cherry.Stage.Emit.JSON.Expression.Operator as Operator
import Cherry.Stage.Emit.JSON.Expression.Pattern as Pattern
import Json.Encode
import Json.Encode.Extra


-- EMITTERS ------------------------------------------------------------------


{-| -}
emit : AST.Expression -> Json.Encode.Value
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
accessEmitter : AST.Expression -> List AST.Accessor -> Json.Encode.Value
accessEmitter expr accessors =
    Json.Encode.Extra.taggedObject "AST.Expression.Access"
        [ ( "expr", emit expr)
        , ( "accessors", Json.Encode.list accessorEmitter accessors)
        ]

{-| -}
accessorEmitter : AST.Accessor -> Json.Encode.Value
accessorEmitter accessor =
    case accessor of
        AST.Computed expr ->
            Json.Encode.Extra.taggedObject "AST.Accessor.Computed"
                [ ( "expr", emit expr )
                ]
        
        AST.Fixed key ->
            Json.Encode.Extra.taggedObject "AST.Accessor.Fixed"
                [ ( "key", Json.Encode.string key )
                ]


-- APPLICATION EMITTER -------------------------------------------------------


{-| -}
applicationEmitter : AST.Expression -> List AST.Expression -> Json.Encode.Value
applicationEmitter func args =
    Json.Encode.Extra.taggedObject "AST.Expression.Application"
        [ ( "func", emit func )
        , ( "args", Json.Encode.list emit args )
        ]


-- INFIX EMITTER -------------------------------------------------------------


{-| -}
infixEmitter : AST.Operator -> AST.Expression -> AST.Expression -> Json.Encode.Value
infixEmitter op lhs rhs =
    Json.Encode.Extra.taggedObject "AST.Expression.Infix"
        [ ( "op", Operator.emit op )
        , ( "lhs", emit lhs )
        , ( "rhs", emit rhs )
        ]


-- CONDITIONAL EMITTER -------------------------------------------------------


{-| -}
conditionalEmitter : AST.Expression -> AST.Expression -> AST.Expression -> Json.Encode.Value
conditionalEmitter predicate true false =
    Json.Encode.Extra.taggedObject "AST.Expression.Conditional"
        [ ( "predicate", emit predicate )
        , ( "true", emit true )
        , ( "false", emit false )
        ]


-- LAMBDA EMITTER ------------------------------------------------------------


{-| -}
lambdaEmitter : List AST.Pattern -> AST.Expression -> Json.Encode.Value
lambdaEmitter args body =
    Json.Encode.Extra.taggedObject "AST.Expression.Lambda"
        [ ( "args", Json.Encode.list (Pattern.emit emit) args )
        , ( "body", emit body )
        ]
