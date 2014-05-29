module Tests where

import Test.HUnit
import qualified AlphaConverter_Tests as AlphaConverter (tests)
import qualified Substitutions_Tests as Substitutions (tests)
import qualified Environments_Tests as Environments (tests)
import qualified Unification_Tests as Unifications (tests)
import qualified TypeInfer_Tests as TypeInfer (tests)
import qualified Ast_Tests as Ast (tests)

main = runTestTT $ TestList [AlphaConverter.tests,
                             Substitutions.tests,
                             Environments.tests,
                             Unifications.tests,
                             TypeInfer.tests,
                             Ast.tests]