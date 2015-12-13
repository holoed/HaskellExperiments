module Compiler.TypeInference.TypeInfer where

import Control.Monad.State

import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set as Set (empty, difference)

import Compiler.Ast
import Compiler.Utils
import Compiler.TypeInference.Environments
import Compiler.TypeInference.TypeTree
import Compiler.TypeInference.AlphaConverter
import Compiler.TypeInference.Unification
import Compiler.TypeInference.Substitutions

newTyVar :: State Int Type
newTyVar = do x <- get
              put (x + 1)
              return (TyVar ("T" ++ show x))

integerCon :: Type
integerCon = TyCon("int", [])

floatCon :: Type
floatCon = TyCon("float", [])

charCon :: Type
charCon = TyCon("char", [])

stringCon :: Type
stringCon = TyCon("string", [])

litToTy :: Literal -> Type
litToTy (Integer _) = integerCon
litToTy (Float _) = floatCon
litToTy (Char _) = charCon
litToTy (String  _) = stringCon


findSc :: String -> Env -> TyScheme
findSc n (Env e) = e |> Map.lookup n |> fromJust

containsSc :: String -> Env -> Bool
containsSc n (Env e) = Map.member n e

addSc :: String -> TyScheme -> Env -> Env
addSc n sc (Env e) = Env (Map.insert n sc e)

-- Calculate the principal type scheme for an expression in a given
-- typing environment
tp :: Env -> Exp -> Type -> Subst -> State Int Subst
tp env e bt s =
        case e of
        (Lit v) -> return (mgu (litToTy v) bt s)
        (Var n) -> do unless (containsSc n env) $ error ("Name " ++ n ++ " not found")
                      let (TyScheme (t, _)) = findSc n env
                      return (mgu (subs t s) bt s)

        (Tuple args) -> do
                          tyArgs <- mapM (const newTyVar) args
                          s1 <- foldM2 (\s' e' t -> tp env e' t s') s args tyArgs
                          let s2 = mgu bt (TyCon ("Tuple", tyArgs)) s1
                          return s2

        (Lam (PVar x, e')) -> do
                          a <- newTyVar
                          b <- newTyVar
                          let s1 = mgu bt (TyLam (a, b)) s
                          let newEnv = addSc x (TyScheme (a, Set.empty)) env
                          tp newEnv e' b s1

        (App(e1, e2)) -> do
                          a <- newTyVar
                          s1 <- tp env e1 (TyLam(a, bt)) s
                          tp env e2 a s1

        (InfixApp(e1, op, e2)) -> do
                                   let exp1 = App(App(Var op, e1), e2)
                                   tp env exp1 bt s

        (Let(name, inV, body)) -> do
                                   a <- newTyVar
                                   s1 <- tp env inV a s
                                   let t = subs a s1
                                   let newScheme = TyScheme (t, getTVarsOfType t `Set.difference` getTVarsOfEnv env)
                                   tp (addSc name newScheme env) body bt s1


predefinedEnv :: Env
predefinedEnv =
    Env([("+", TyScheme (TyLam(integerCon, TyLam(integerCon, integerCon)), Set.empty)),
         ("*", TyScheme (TyLam(integerCon, TyLam(integerCon, integerCon)), Set.empty)),
         ("-", TyScheme (TyLam(integerCon, TyLam(integerCon, integerCon)), Set.empty))
           ] |> Map.fromList)

typeOf :: Exp -> Type
typeOf e =
   evalState typeOf' 0 |> renameTVarsToLetters
   where typeOf' = do
                     a <- newTyVar
                     let emptySubst = Subst Map.empty
                     s1 <- tp predefinedEnv e a emptySubst
                     return (subs a s1)
