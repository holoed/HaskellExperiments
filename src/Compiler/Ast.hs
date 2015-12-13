module Compiler.Ast where

import Text.Printf

data Literal = Char     Char
             | String   String
             | Integer  Integer
             | Float    Float deriving Show

data PExp = PVar String deriving Show

data Exp = Var String
         | Lam (PExp, Exp)
         | Tuple [Exp]
         | App (Exp, Exp)
         | InfixApp (Exp, String, Exp)
         | Let (String, Exp, Exp)
         | Lit Literal deriving Show

litToString :: Literal -> String
litToString (Char ch) = show ch
litToString (String s) = s
litToString (Integer i) = show i
litToString (Float f) = show f

patToString :: PExp -> String
patToString (PVar s) = s

expToString :: Exp -> String
expToString (Var s) = s
expToString (Tuple s) = printf "(%s)" (foldl (\acc e -> printf "%s, %s" acc (expToString e)) (expToString (head s)) (tail s))
expToString (Lam (p, e)) = printf "(\\%s -> %s)" (patToString p) (expToString e)
expToString (App (e1, e2)) = printf "(%s %s)" (expToString e1) (expToString e2)
expToString (InfixApp (e1, op, e2)) = printf "(%s %s %s)" (expToString e1) op (expToString e2)
expToString (Let (v, e1, e2)) = printf "let %s = %s in %s" v (expToString e1) (expToString e2)
expToString (Lit x) = litToString x
