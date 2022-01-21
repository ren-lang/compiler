module Control.ResultM exposing (..)

{-| -}

-- TYPES -----------------------------------------------------------------------


{-| -}
type alias ResultM ctx e a =
    ctx -> ( ctx, Result e a )



-- RUNNING ---------------------------------------------------------------------


{-| -}
runM : ctx -> ResultM ctx e a -> Result e a
runM ctx infer =
    infer ctx |> Tuple.second



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
succeed : a -> ResultM ctx e a
succeed a =
    \context -> ( context, Ok a )


{-| -}
fail : e -> ResultM ctx e a
fail e =
    \context -> ( context, Err e )


{-| -}
join : ResultM ctx e (ResultM ctx e a) -> ResultM ctx e a
join =
    andThen identity



-- ERROR HANDLING --------------------------------------------------------------


{-| -}
catch : (e -> ResultM ctx e a) -> ResultM ctx e a -> ResultM ctx e a
catch handleE runA =
    \context ->
        case runA context of
            ( ctx, Ok a ) ->
                ( ctx, Ok a )

            ( ctx, Err e ) ->
                handleE e ctx


{-| -}
unwrapMaybe : e -> ResultM ctx e (Maybe a) -> ResultM ctx e a
unwrapMaybe error runA =
    \context ->
        case runA context of
            ( ctx, Ok (Just a) ) ->
                ( ctx, Ok a )

            ( ctx, Ok Nothing ) ->
                ( ctx, Err error )

            ( ctx, Err e ) ->
                ( ctx, Err e )



-- TRANSFORMING INFERRED VALUES ------------------------------------------------


{-| -}
map : (a -> b) -> ResultM ctx e a -> ResultM ctx e b
map f runA =
    \context ->
        runA context
            |> Tuple.mapSecond (Result.map f)


{-| -}
map2 : (a -> b -> c) -> ResultM ctx e a -> ResultM ctx e b -> ResultM ctx e c
map2 f runA runB =
    succeed f
        |> andMap runA
        |> andMap runB


{-| -}
mapM : (a -> ResultM ctx e b) -> List a -> ResultM ctx e (List b)
mapM f xs =
    map List.reverse <|
        List.foldl (map2 (::) << f) (succeed []) xs



--


sequence : List (ResultM ctx e a) -> ResultM ctx e (List a)
sequence =
    List.foldr (map2 (::)) (succeed [])



-- CHAINING INFERENCE ----------------------------------------------------------


{-| -}
andThen : (a -> ResultM ctx e b) -> ResultM ctx e a -> ResultM ctx e b
andThen f runA =
    \context ->
        case runA context of
            ( ctx, Ok a ) ->
                f a ctx

            ( ctx, Err e ) ->
                ( ctx, Err e )


do : ResultM ctx e a -> (a -> ResultM ctx e b) -> ResultM ctx e b
do runA f =
    andThen f runA


{-| -}
andMap : ResultM ctx e a -> ResultM ctx e (a -> b) -> ResultM ctx e b
andMap runA runF =
    \context ->
        case runF context of
            ( ctx, Ok f ) ->
                map f runA ctx

            ( ctx, Err e ) ->
                ( ctx, Err e )
