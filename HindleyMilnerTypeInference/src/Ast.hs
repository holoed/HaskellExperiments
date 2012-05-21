module Ast where

data Literal = Char     Char
             | String   String
             | Integer  Integer
             | Float    Float deriving Show

data Exp = Var String
         | Lam (String, Exp)
         | App (Exp, Exp)
         | InfixApp (Exp, String, Exp)
         | Let (String, Exp, Exp)
         | Lit Literal deriving Show    
         
litToString :: Literal -> String
litToString (Char ch) = show ch
litToString (String s) = s
litToString (Integer i) = show i
litToString (Float f) = show f          
         
expToString :: Exp -> String
expToString (Var s) = s
expToString (Lam (arg, e)) = "(\\" ++ arg ++ " -> " ++ (expToString e) ++ ")"   
expToString (App (e1, e2)) = "(" ++ (expToString e1) ++ " " ++ (expToString e2) ++ ")"
expToString (InfixApp (e1, op, e2)) = "(" ++ (expToString e1) ++ " " ++ op ++ " " ++ (expToString e2) ++ ")"
expToString (Let (v, e1, e2)) = "let " ++ v ++ " = " ++ (expToString e1) ++ " in " ++ (expToString e2)
expToString (Lit x) = litToString x      