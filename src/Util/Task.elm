module Util.Task exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Task exposing (Task)



-- CONSTRUCTORS ----------------------------------------------------------------


traverse : (a -> Task x b) -> List a -> Task x (List b)
traverse f xs =
    Task.sequence <| List.map f xs



-- MANIPULATIONS ---------------------------------------------------------------


replace : b -> Task x a -> Task x b
replace b task =
    Task.map (Basics.always b) task
