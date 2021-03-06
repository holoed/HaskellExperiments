module Compiler.TypeInference.AlphaConverter_Tests (tests) where

import Control.Monad.Trans.State
import qualified Data.Map as Map
import Compiler.TypeInference.TypeTree
import Compiler.TypeInference.AlphaConverter ( getName, renameTVarsToLetters )
import Test.HUnit

stateTest m t xs = evalState m (Map.fromList xs, t)

{- getName Tests -}
shouldFindSymbol::Test
shouldFindSymbol = TestCase $ assertEqual
  "Should get type of symbol if found" expected actual
  where
    expected = 'a'
    actual = stateTest (getName "x") 'a' []

shouldFindSymbol2::Test
shouldFindSymbol2 = TestCase $ assertEqual
  "Should get type of symbol if found" expected actual
  where
    expected = 'b'
    actual = evalState (do _ <- getName "x";
                           getName "y") (Map.fromList [], 'a')

shouldFindSymbol3::Test
shouldFindSymbol3 = TestCase $ assertEqual
  "Should get type of symbol if found" expected actual
  where
    expected = 'c'
    actual = evalState (do _ <- getName "x";
                           _ <- getName "y";
                           getName "z") (Map.fromList [], 'a')

shouldReturnBaseType::Test
shouldReturnBaseType = TestCase $ assertEqual
  "Should return the type passed in if the symbol is not found" expected actual
  where
    expected = 'e'
    actual = stateTest (getName "x") 'a' [("x", 'e')]

shouldNotFindSymbol::Test
shouldNotFindSymbol = TestCase $ assertEqual
  "Should get type of symbol if found" expected actual
  where
    expected = 'b'
    actual = stateTest (getName "y") 'b' [("x", 'a')]

{- renameTVarsToLetters Tests -}

shouldRenameTyVar::Test
shouldRenameTyVar = TestCase $ assertEqual
  "Should rename TyVar to name type" expected actual
  where
    expected = TyVar "a"
    actual = renameTVarsToLetters (TyVar "T0")

shouldRenameTyTuple::Test
shouldRenameTyTuple = TestCase $ assertEqual
  "Should rename TyTuple to name type" expected actual
  where
    expected = TyCon ("Tuple", [TyVar "a", TyVar "b"])
    actual = renameTVarsToLetters (TyCon ("Tuple", [TyVar "T0", TyVar "T1"]))

shouldRenameTyLam::Test
shouldRenameTyLam = TestCase $ assertEqual
  "Should rename TyLam to name type" expected actual
  where
    expected = TyLam (TyVar "a", TyVar "b")
    actual = renameTVarsToLetters (TyLam (TyVar "T0", TyVar "T1"))

shouldRenameTyCon::Test
shouldRenameTyCon = TestCase $ assertEqual
  "Should rename TyCon to name type" expected actual
  where
    expected= TyCon ("List", [TyVar "a"])
    actual = renameTVarsToLetters (TyCon ("List", [TyVar "T0"]))

tests:: Test
tests = TestList [shouldFindSymbol,
                  shouldFindSymbol2,
                  shouldFindSymbol3,
                  shouldReturnBaseType,
                  shouldNotFindSymbol,
                  shouldRenameTyVar,
                  shouldRenameTyTuple,
                  shouldRenameTyLam,
                  shouldRenameTyCon]
