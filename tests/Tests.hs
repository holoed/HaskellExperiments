module Main where

import Test.HUnit
import qualified Compiler.TypeInference.AlphaConverter_Tests as AlphaConverter (tests)
import qualified Compiler.TypeInference.Substitutions_Tests as Substitutions (tests)
import qualified Compiler.TypeInference.Environments_Tests as Environments (tests)
import qualified Compiler.TypeInference.Unification_Tests as Unifications (tests)
import qualified Compiler.TypeInference.TypeInfer_Tests as TypeInfer (tests)
import qualified Compiler.Ast_Tests as Ast (tests)

main :: IO Counts
main = runTestTT $ TestList [AlphaConverter.tests,
                             Substitutions.tests,
                             Environments.tests,
                             Unifications.tests,
                             TypeInfer.tests,
                             Ast.tests]
