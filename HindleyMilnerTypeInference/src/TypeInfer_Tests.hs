module TypeInfer_Tests (tests) where

import Ast
import TypeTree
import TypeInfer
import Test.HUnit

typeOfTest1 = TestCase $ assertEqual 
	"type of identity function" expected actual
	where
		expected = TyLam (TyVar "a", TyVar "a")
		actual = typeOf (Let ("id", Lam(PVar "x", Var "x"), Var "id"))

typeofTest2 = TestCase $ assertEqual
	"type of function that drops first argument" expected actual
	where
		expected = TyLam (TyVar "a", TyLam (TyVar "b", TyVar "b"))
		actual = typeOf (Let ("f", Lam (PVar "x", Lam (PVar "y", Var "y")), Var "f"))

typeofTest3 = TestCase $ assertEqual
	"type of let value" expected actual
	where
		expected = TyCon ("int", [])
		actual = typeOf (Let ("x", Lit(Integer 42), Var "x"))

typeofTest4 = TestCase $ assertEqual
	"type of let tuple value" expected actual
	where
		expected = TyCon ("Tuple", [TyCon ("string", []), TyCon ("int", [])])
		actual = typeOf (Let ("x", Tuple [Lit(String "Hello"), Lit (Integer 42)], Var "x"))

tests = TestList [typeOfTest1,
                  typeofTest2,
                  typeofTest3,
                  typeofTest4]