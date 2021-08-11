module Ren.Language.Module exposing
    ( imports, exports
    , references, referencesNamespace, referencesQualified
    )

{-|

@docs imports, exports
@docs references, referencesNamespace, referencesQualified

-}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Language exposing (..)
import Ren.Language.Declaration



-- QUERIES ---------------------------------------------------------------------


imports : String -> Module -> Bool
imports name module_ =
    module_.imports
        |> List.concatMap .bindings
        |> List.any ((==) name)


exports : String -> Module -> Bool
exports name module_ =
    module_.declarations
        |> List.any
            (\( visibility, declaration ) ->
                case ( visibility, declaration ) of
                    ( Public, Function n _ _ ) ->
                        name == n

                    ( Public, Variable n _ ) ->
                        exportsInPattern name n

                    _ ->
                        False
            )


exportsInPattern : String -> Pattern -> Bool
exportsInPattern name pattern =
    case pattern of
        ArrayDestructure elements ->
            List.any (exportsInPattern name) elements

        Name n ->
            name == n

        ObjectDestructure entries ->
            List.any
                (\( k, v ) ->
                    case v of
                        Just p ->
                            exportsInPattern name p

                        Nothing ->
                            name == k
                )
                entries

        VariantDestructure _ _ ->
            False

        Value _ ->
            False

        Wildcard _ ->
            False


references : String -> Module -> Bool
references name module_ =
    List.any
        (Tuple.second >> Ren.Language.Declaration.references name)
        module_.declarations


referencesNamespace : List String -> Module -> Bool
referencesNamespace namespace module_ =
    List.any
        (Tuple.second >> Ren.Language.Declaration.referencesNamespace namespace)
        module_.declarations


referencesQualified : List String -> String -> Module -> Bool
referencesQualified namespace name module_ =
    List.any
        (Tuple.second >> Ren.Language.Declaration.referencesQualified namespace name)
        module_.declarations
