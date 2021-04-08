module Cherry.Stage.Generate.JSON.Expression exposing 
    ( run, generator 
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Generate.JSON.Expression.Identifier as Identifier
import Cherry.Stage.Generate.JSON.Expression.Literal as Literal
import Cherry.Stage.Generate.JSON.Expression.Operator as Operator
import Cherry.Stage.Generate.JSON.Expression.Pattern as Pattern
import Json.Encode
import Json.Encode.Extra


-- RUNNING THE GENERATOR -------------------------------------------------------


{-| -}
run : AST.Expression -> String
run ast =
    generator ast
        |> Json.Encode.encode 4


-- GENERATORS ------------------------------------------------------------------


{-| -}
generator : AST.Expression -> Json.Encode.Value
generator expression =
    case expression of
        AST.Access expr accessors ->
            accessGenerator expr accessors

        AST.Application func args ->
            applicationGenerator func args

        AST.Identifier identifier ->
            Identifier.generator identifier

        AST.Infix op lhs rhs ->
            infixGenerator op lhs rhs

        AST.Conditional predicate true false ->
            conditionalGenerator predicate true false

        AST.Lambda args body ->
            lambdaGenerator args body

        AST.Literal literal ->
            Literal.generator generator literal


-- ACCESS GENERATORS -----------------------------------------------------------


{-| -}
accessGenerator : AST.Expression -> List AST.Accessor -> Json.Encode.Value
accessGenerator expr accessors =
    Json.Encode.Extra.taggedObject "AST.Expression.Access"
        [ ( "expr", generator expr)
        , ( "accessors", Json.Encode.list accessorGenerator accessors)
        ]

{-| -}
accessorGenerator : AST.Accessor -> Json.Encode.Value
accessorGenerator accessor =
    case accessor of
        AST.Computed expr ->
            Json.Encode.Extra.taggedObject "AST.Accessor.Computed"
                [ ( "expr", generator expr )
                ]
        
        AST.Fixed key ->
            Json.Encode.Extra.taggedObject "AST.Accessor.Fixed"
                [ ( "key", Json.Encode.string key )
                ]


-- APPLICATION GENERATOR -------------------------------------------------------


{-| -}
applicationGenerator : AST.Expression -> List AST.Expression -> Json.Encode.Value
applicationGenerator func args =
    Json.Encode.Extra.taggedObject "AST.Expression.Application"
        [ ( "func", generator func )
        , ( "args", Json.Encode.list generator args )
        ]


-- INFIX GENERATOR -------------------------------------------------------------


{-| -}
infixGenerator : AST.Operator -> AST.Expression -> AST.Expression -> Json.Encode.Value
infixGenerator op lhs rhs =
    Json.Encode.Extra.taggedObject "AST.Expression.Infix"
        [ ( "op", Operator.generator op )
        , ( "lhs", generator lhs )
        , ( "rhs", generator rhs )
        ]


-- CONDITIONAL GENERATOR -------------------------------------------------------


{-| -}
conditionalGenerator : AST.Expression -> AST.Expression -> AST.Expression -> Json.Encode.Value
conditionalGenerator predicate true false =
    Json.Encode.Extra.taggedObject "AST.Expression.Conditional"
        [ ( "predicate", generator predicate )
        , ( "true", generator true )
        , ( "false", generator false )
        ]


-- LAMBDA GENERATOR ------------------------------------------------------------


{-| -}
lambdaGenerator : List AST.Pattern -> AST.Expression -> Json.Encode.Value
lambdaGenerator args body =
    Json.Encode.Extra.taggedObject "AST.Expression.Lambda"
        [ ( "args", Json.Encode.list (Pattern.generator generator) args )
        , ( "body", generator body )
        ]
