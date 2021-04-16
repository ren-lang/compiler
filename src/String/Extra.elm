module String.Extra exposing (..)


{-| -}
indent : Int -> Int -> String -> String
indent size n s =
    String.split "\n" s
        |> List.map ((++) (String.repeat (size * n) " "))
        |> String.join "\n"

{-| -}
nest : Int -> Int -> String -> String
nest size n s =
    case String.split "\n" s of
        [] ->
            ""

        line :: [] ->
            line

        line :: rest ->
            List.map ((++) (String.repeat (size * n) " ")) rest
                |> String.join "\n"
                |> (++) (line ++ "\n")

{-| -}
drop : Int -> Int -> String -> String
drop size n s =
    "\n" ++ indent size n s