module Ren.Compiler.Optimise.Declaration exposing 
    ( optimise
    , removeUnusedBindings
    , simplifyBindings
    , simplifyBody
    )


{-| 

@docs run
@docs remuveUnusedBindings, simplifyBindings, simplifyBody

-}


-- IMPORTS ---------------------------------------------------------------------


import Maybe.Extra
import Ren.Compiler.Optimise.Expression as Expression
import Ren.Data.Declaration exposing (Declaration(..))
import Ren.Data.Declaration.Binding exposing (Binding(..))
import Ren.Data.Expression as Expression exposing (Expression)
import Ren.Data.Expression.Pattern exposing (Pattern(..))


-- RUNNING OPTIMISATIONS -------------------------------------------------------


{-| -}
optimise : Declaration -> Declaration
optimise =
    apply 
        [ simplifyBody
        , simplifyBindings
        , removeUnusedBindings
        ]

{-| -}
apply : List (Declaration -> Maybe Declaration) -> Declaration -> Declaration
apply optimisations declaration =
    case optimisations of
        head :: tail ->
            List.foldr Maybe.Extra.andThenAttempt (head declaration) tail
                |> Maybe.map (apply optimisations)
                |> Maybe.withDefault declaration

        [] ->
            declaration


-- OPTIMISATIONS: SIMPLIFYING BINDINGS -----------------------------------------


{-| -}
simplifyBindings : Declaration -> Maybe (Declaration)
simplifyBindings declaration =
    case declaration of
        Function ({ bindings } as data) ->
            List.map simplifyBinding bindings |> (\simplifiedBindings ->
                if simplifiedBindings == bindings then
                    Nothing

                else
                    Just <| Function { data | bindings = simplifiedBindings }
            )
        
        Variable ({ bindings } as data) ->
            List.map simplifyBinding bindings |> (\simplifiedBindings ->
                if simplifiedBindings == bindings then
                    Nothing

                else
                    Just <| Variable { data | bindings = simplifiedBindings }
            )

{-| -}
simplifyBinding : Binding -> Binding
simplifyBinding (Binding pattern expr) =
    Binding pattern (Expression.optimise expr)


-- OPTIMISATIONS: SIMPLIFYING BODY ---------------------------------------------


{-| -}
simplifyBody : Declaration -> Maybe Declaration
simplifyBody declaration =
    case declaration of
        Function ({ body } as data) ->
            Expression.optimise body |> (\simplifiedBody ->
                if simplifiedBody == body then
                    Nothing

                else
                    Just <| Function { data | body = simplifiedBody }
            )

        Variable ({ body } as data) ->
            Expression.optimise body |> (\simplifiedBody ->
                if simplifiedBody == body then
                    Nothing

                else
                    Just <| Variable { data | body = simplifiedBody }
            )


-- OPTIMISATIONS: REMOVE UNUSED BINDINGS ---------------------------------------


{-| -}
removeUnusedBindings : Declaration -> Maybe Declaration
removeUnusedBindings declaration =
    case declaration of
        Function ({ bindings, body } as data) ->
            List.filter (isBindingUsed body bindings) bindings |> (\filteredBindings ->
                if filteredBindings == bindings then
                    Nothing

                else
                    Just <| Function { data | bindings = filteredBindings }
            )

        Variable ({ bindings, body } as data) ->
            List.filter (isBindingUsed body bindings) bindings |> (\filteredBindings ->
                if filteredBindings == bindings then
                    Nothing

                else
                    Just <| Variable { data | bindings = filteredBindings }
            )


{-| -}
isBindingUsed : Expression -> (List Binding) -> Binding -> Bool
isBindingUsed body bindings (Binding pattern _) =
    isPatternUsed body pattern
        || List.any (\(Binding _ expr) -> isPatternUsed expr pattern) bindings

{-| -}
isPatternUsed : Expression -> Pattern Expression -> Bool
isPatternUsed body pattern =
    case pattern of
        ArrayDestructure patterns ->
            List.any (isPatternUsed body) patterns

        Name name ->
            Expression.referencesName name body

        ObjectDestructure fields ->
            List.any
                (\( key, val ) ->
                    if val == Nothing then
                        Expression.referencesName key body

                    else
                        Maybe.map (isPatternUsed body) val
                            |> Maybe.withDefault False
                )
                fields

        Value _ ->
            False

        -- Wildcard bindings may be used as a way of performing side effects
        -- and so its safer to just pretend that they're used even when they
        -- *cannot* be used.
        Wildcard _ ->
            True