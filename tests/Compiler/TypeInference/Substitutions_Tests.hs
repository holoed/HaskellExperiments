module Compiler.TypeInference.Substitutions_Tests (tests) where

import Compiler.TypeInference.TypeTree
import Compiler.TypeInference.Substitutions
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Test.HUnit

substToList (Subst m) = Map.toList m

{-  extend Tests -}

extendTest = TestCase $ assertEqual
    "Should extend environment with new substitution" (substToList expected) (substToList actual)
    where
    	expected = Subst (Map.fromList ([("a", TyCon ("Int", []))]))
    	actual = extend "a" (TyCon ("Int", [])) (Subst Map.empty)

extendTwiceTest = TestCase $ assertEqual
	"Should extend environment with two substitutions" (substToList expected) (substToList actual)
	where
		expected = Subst (Map.fromList ([("a", TyCon ("Int", [])), ("b", TyCon ("String", []))]))
		actual = extend "b" (TyCon ("String", [])) (extend "a" (TyCon ("Int", [])) (Subst Map.empty))

{- lookup Tests -}
lookupTest = TestCase $ assertEqual
    "Should return new type if substitution is not found" expected actual
    where
    	expected = TyVar "a"
    	actual = lookup "a" (Subst Map.empty)

lookupTest2 = TestCase $ assertEqual
	"Should return substitution type if found" expected actual
	where
		expected = TyCon ("Int", [])
		actual = lookup "a" (Subst (Map.fromList [("a", TyCon ("Int", []))]))

{- subst Tests -}
subsTest = TestCase $ assertEqual
	"Should substitute type variable with resolved type in substitutions" expected actual
	where
		expected = TyCon ("Int", [])
		actual = subs (TyVar "a") (Subst (Map.fromList [("a", TyCon ("Int", []))]))

subsTest2 = TestCase $ assertEqual
	 "Should return input type if is the same as the resolved type" expected actual
	 where
	 	expected = TyCon ("Int", [])
	 	actual = subs (TyCon ("Int", [])) (Subst (Map.fromList [("Int", TyCon ("Int", []))]))

subsTest3 = TestCase $ assertEqual
	 "Should resolve type nested in a lambda" expected actual
	 where
	 	expected = TyLam (TyCon ("Int", []), TyCon ("String", []))
	 	actual = subs (TyLam (TyVar "a", TyVar "b")) (Subst (Map.fromList [("a", TyCon ("Int", [])), ("b", TyCon ("String", []))]))

subsTest4 = TestCase $ assertEqual
      "Should resolve type constructor parameters" expected actual
      where
      	expected = TyCon ("List", [TyCon ("Int", [])])
      	actual = subs (TyCon ("List", [TyVar "a"])) (Subst (Map.fromList [("a", TyCon ("Int", []))]))

subsTest5 = TestCase $ assertEqual
      "Should resolve type constructor with more than one parameter" expected actual
      where
      	expected = TyCon ("Tuple", [TyCon ("String", []), TyCon ("Int", [])])
      	actual = subs (TyCon ("Tuple", [TyVar "a", TyVar "b"])) (Subst (Map.fromList [("a", TyCon ("String", [])), ("b", TyCon ("Int", []))]))

tests = TestList [extendTest,
			      extendTwiceTest,
			      lookupTest,
			      lookupTest2,
			      subsTest,
			      subsTest2,
			      subsTest3,
			      subsTest4,
			      subsTest5]
