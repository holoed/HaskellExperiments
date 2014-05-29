module Main where

import Ast
import TypeTree
import TypeInfer
import Utils

composeAst :: Exp
composeAst = Let("compose",
                    Lam(PVar "f",
                        Lam(PVar "g",
                            Lam (PVar "x",
                                App(Var "g", App(Var "f", Var "x"))))),
                        Var "compose")

t :: Type
t = typeOf composeAst
 
main::IO()
main = do 
       (composeAst |> expToString
                   |> putStrLn)
       (t |> typeToString
          |> putStrLn) 

-- Local Variables:
-- compile-command: "ghc -o HindleyMilner Main.hs && ./HindleyMilner"
-- End: