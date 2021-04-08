module Cherry.Stage.Generate.JavaScript exposing 
    ( run
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST


-- RUNNING THE GENERATOR -------------------------------------------------------


run : AST.Module -> String
run ast =
    Debug.todo "Generate JavaScript source code."
