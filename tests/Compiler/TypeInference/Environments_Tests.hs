module Compiler.TypeInference.Environments_Tests (tests) where

import Compiler.TypeInference.Environments
import Compiler.TypeInference.TypeTree
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit

{- getTVarsOfType Tests -}

getTVarsOfTypeTest1 = TestCase $ assertEqual
	"Should return all type variables occurring in a type" expected actual
	where
		expected = Set.fromList ["a"]
		actual = getTVarsOfType (TyVar "a")

getTVarsOfTypeTest2 = TestCase $ assertEqual
    "Should return all type variables occurring in a lambda with same type for dom and codom" expected actual
    where
    	expected = Set.fromList ["a"]
    	actual = getTVarsOfType (TyLam (TyVar "a", TyVar "a"))

getTVarsOfTypeTest3 = TestCase $ assertEqual
     "Should return all type variables occurring in a lambda with different types for dom and codom" expected actual
     where
     	expected = Set.fromList ["a", "b"]
     	actual = getTVarsOfType (TyLam (TyVar "a", TyVar "b"))

getTVarsOfTypeTest4 = TestCase $ assertEqual
     "Should return all type variables of a type constructor" expected actual
     where
     	expected = Set.fromList ["a"]
     	actual = getTVarsOfType (TyCon ("List", [TyVar "a"]))

getTVarsOfTypeTest5 = TestCase $ assertEqual
     "Should return all type variables of a type constructor" expected actual
     where
     	expected = Set.fromList ["a", "b"]
     	actual = getTVarsOfType (TyCon ("Tuple", [TyVar "a", TyVar "b"]))

{- getTVarsOfScheme Tests -}

getTVarsOfSchemeTest1 = TestCase $ assertEqual
      "Should return all type variables in Type Scheme" expected actual
      where
      	expected = Set.fromList ["a"]
      	actual = getTVarsOfScheme (TyScheme (TyVar "a", Set.empty))

getTVarsOfSchemeTest2 = TestCase $ assertEqual
       "Should return all type variables in a Type Scheme with some already used." expected actual
       where
       	expected = Set.fromList []
       	actual = getTVarsOfScheme (TyScheme (TyVar "a", Set.fromList ["a"]))

tests = TestList [getTVarsOfTypeTest1,
                  getTVarsOfTypeTest2,
                  getTVarsOfTypeTest3,
                  getTVarsOfTypeTest4,
                  getTVarsOfTypeTest5,
                  getTVarsOfSchemeTest1,
                  getTVarsOfSchemeTest2]
