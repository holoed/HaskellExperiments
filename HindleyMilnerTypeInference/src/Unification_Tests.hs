module Unification_Tests (tests) where

import TypeTree
import Substitutions
import Unification
import qualified Data.Map as Map
import Test.HUnit 

substToList (Subst m) = Map.toList m

assertAreEqual m x y = assertEqual m (substToList x) (substToList y)

mostGeneralUnifierTypeTest1 = TestCase $ assertAreEqual
	"Should find most general unifier type" expected actual
	where
		expected = Subst (Map.fromList [("a", TyCon ("Int", []))])
		actual = mgu (TyVar "a") (TyVar "a") expected

mostGeneralUnifierTypeTest2 = TestCase $ assertAreEqual
	"Should extend environment if two types don't match" expected actual
	where
		expected = Subst (Map.fromList [("a", TyVar "b")])
		actual = mgu (TyVar "a") (TyVar "b") (Subst (Map.empty))

mostGeneralUnifierTypeTest3 = TestCase $ assertAreEqual
	"Should extend environment if two types don't match" expected actual
	where
		expected = Subst (Map.fromList [("a", TyCon ("Int", []))])
		actual = mgu (TyCon ("Int", [])) (TyVar "a") (Subst (Map.empty))

mostGeneralUnifierTypeTest4 = TestCase $ assertAreEqual
	"Should extend environment if two types don't match" expected actual
	where
		expected = Subst (Map.fromList [("a", TyCon ("Int", []))])
		actual = mgu (TyVar "a") (TyCon ("Int", [])) (Subst (Map.empty))

mostGeneralUnifierTypeTest5 = TestCase $ assertAreEqual
	"Most general unifier type between two lambdas"  expected actual
	where
		expected = Subst (Map.fromList [("a", TyVar "c"), ("b", TyVar "d")])
		actual = mgu (TyLam (TyVar "a", TyVar "b")) (TyLam (TyVar "c", TyVar "d")) (Subst (Map.empty))

mostGeneralUnifierTypeTest6 = TestCase $ assertAreEqual
	"Most general unifier between two container types" expected actual
	where
		expected = Subst (Map.fromList [("a", TyVar "b")])
		actual = mgu (TyCon ("List", [TyVar "a"])) (TyCon ("List", [TyVar "b"])) (Subst (Map.empty))

mostGeneralUnifierTypeTest7 = TestCase $ assertAreEqual 
	"Most general unifier between two container types with more than one type argument" expected actual
	where
		expected = Subst (Map.fromList [("a", TyVar "b"), ("c", TyVar "d")])
		actual = mgu (TyCon ("Tuple", [TyVar "a", TyVar "c"])) (TyCon ("Tuple", [TyVar "b", TyVar "d"])) (Subst (Map.empty))

tests = TestList [mostGeneralUnifierTypeTest1,
                  mostGeneralUnifierTypeTest2,
                  mostGeneralUnifierTypeTest3,
                  mostGeneralUnifierTypeTest4,
                  mostGeneralUnifierTypeTest5,
                  mostGeneralUnifierTypeTest6,
                  mostGeneralUnifierTypeTest7]