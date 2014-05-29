module Ast_Tests (tests) where

import Ast
import Test.HUnit


expToStringTest1 = TestCase $ assertEqual
	"Should return var name" expected actual
	where
		expected = "x"
		actual = expToString (Var "x")

expToStringTest2 = TestCase $ assertEqual
	"Should print out tuple as string" expected actual
	where
		expected = "(x, y)"
		actual = expToString (Tuple [Var "x", Var "y"])

expToStringTest3 = TestCase $ assertEqual
	"Should primt out lambda as a string" expected actual
	where
		expected = "(\\x -> x)"
		actual = expToString (Lam (PVar "x", Var "x"))

expToStringTest4 = TestCase $ assertEqual
	"Should print out function application as a string" expected actual
	where
		expected = "(f x)"
		actual = expToString (App (Var "f", Var "x"))

expToStringTest5 = TestCase $ assertEqual
	"Should print infix application as a string" expected actual
	where
		expected = "(x + y)"
		actual = expToString (InfixApp (Var "x", "+", Var "y"))

expToStringTest6 = TestCase $ assertEqual
	"Should print out let expression as a string" expected actual
	where
		expected = "let x = 42 in x"
		actual = expToString (Let ("x", Lit (Integer 42), Var "x"))

expToStringTest7 = TestCase $ assertEqual
	"Should print out let expression containing a function" expected actual
	where
		expected = "let f = (\\x -> (x + 1)) in f"
		actual = expToString (Let ("f", Lam (PVar "x", InfixApp (Var "x", "+", Lit (Integer 1))), Var "f"))

tests = TestList [expToStringTest1,
                  expToStringTest2,
                  expToStringTest3,
                  expToStringTest4,
                  expToStringTest5,
                  expToStringTest6,
                  expToStringTest7]