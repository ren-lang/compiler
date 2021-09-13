module List.Extra exposing (..)


at : Int -> List a -> Maybe a
at idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
