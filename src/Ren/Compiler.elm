module Ren.Compiler exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Ren.Compiler.Emit.ESModule as Emit
import Ren.Compiler.Optimise as Optimise
import Ren.Compiler.Parse as Parse
import Ren.Compiler.Transform as Transform


{-| -}
run : String -> Maybe String
run =
    Parse.run
        >> Result.map Transform.runAll
        >> Result.map Optimise.runAll
        >> Result.map Emit.run
        >> Result.toMaybe
