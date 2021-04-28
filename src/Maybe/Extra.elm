module Maybe.Extra exposing (..)

{-| -}
or : Maybe a -> Maybe a -> Maybe a
or b a = if a == Nothing then b else a

{-| -}
andThenAttempt : (a -> Maybe a) -> Maybe a -> Maybe a
andThenAttempt f a = a |> Maybe.andThen f |> or a