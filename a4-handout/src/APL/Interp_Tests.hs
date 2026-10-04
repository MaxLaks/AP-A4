module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      -- Task 2:
      testCase "put then get" $
        runEval
          (do
            evalKvPut (ValInt 67) (ValInt 420)
            evalKvGet (ValInt 67)
          )
          @?= ([], Right (ValInt 420)),
      ---
      testCase "overwrite an existing key" $
        runEval
          (do
            evalKvPut (ValInt 67) (ValInt 420)
            evalKvPut (ValInt 67) (ValBool False)
            evalKvGet (ValInt 67)
          )
          @?= ([], Right (ValBool False)),
      ---
      testCase "overwrite preserves other keys" $
        runEval
          (do
            evalKvPut (ValInt 1) (ValInt 10)
            evalKvPut (ValInt 2) (ValInt 20)
            evalKvPut (ValInt 1) (ValInt 30)
            first <- evalKvGet (ValInt 1)
            second <- evalKvGet (ValInt 2)
            pure (first, second)
          )
          @?= ([], Right (ValInt 30, ValInt 20)),
      ---
      testCase "missing key" $
        runEval (evalKvGet (ValInt 99))
          @?= ([], Left "Non-existing key: ValInt 99"),
      ---
      testCase "get preserves the entry and continues" $
        runEval
          (do
            evalKvPut (ValBool True) (ValInt 42)
            first <- evalKvGet (ValBool True)
            evalPrint "Retrieved"
            second <- evalKvGet (ValBool True)
            pure (first, second)
          )
          @?= (["Retrieved"], Right (ValInt 42, ValInt 42)),
      ---
      testCase "missing key stops execution but keeps earlier output" $
        runEval
          (do
            evalPrint "Before lookup"
            value <- evalKvGet (ValInt 99)
            evalPrint "After lookup"
            pure value
          )
          @?= (["Before lookup"], Left "Non-existing key: ValInt 99"),
      -- Task 4:
      testCase "Break outside loop" $
        eval' (Break (CstBool True))
          @?= ([], Left "Break outside loop"),
      --
      testCase "Break stops loop immediately" $
        eval'
          ( ForLoop
              ("p", CstInt 0)
              ("i", CstInt 100)
              (Let "_" (Break (CstBool True)) (Var "i"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Loop without break runs to completion" $
        eval'
          ( ForLoop
              ("p", CstInt 0)
              ("i", CstInt 5)
              (Add (Var "p") (Var "i"))
          )
          @?= ([], Right (ValInt 10)), 
      --
      testCase "Break on a later iteration" $
        eval'
          ( ForLoop
              ("p", CstInt 0)
              ("i", CstInt 100)
              ( If
                  (Eql (Var "i") (CstInt 3))
                  (Break (Var "p"))
                  (Add (Var "p") (Var "i"))
              )
          )
          @?= ([], Right (ValInt 3)), -- 0+1+2, breaks when i=3
      --
      testCase "Inner loop's break doesn't escape to outer loop" $
        eval'
          ( ForLoop
              ("outer_p", CstInt 0)
              ("outer_i", CstInt 3)
              ( ForLoop
                  ("inner_p", CstInt 0)
                  ("inner_i", CstInt 100)
                  ( If
                      (Eql (Var "inner_i") (CstInt 2))
                      (Break (Var "inner_p"))
                      (Var "inner_p")
                  )
              )
          )
          @?= ([], Right (ValInt 0)), -- outer loop runs all 3 iterations fine
      --
      testCase "localEnv propagates into a nested loop body" $
        eval'
          ( Let
              "x"
              (CstInt 10)
              ( ForLoop
                  ("p", CstInt 0)
                  ("i", CstInt 1)
                  (Add (Var "p") (Var "x"))
              )
          )
          @?= ([], Right (ValInt 10))
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
        --
      -- Task 2:
      testCase "IO put then get" $ do
        result <- runEvalIO $ do
          evalKvPut (ValInt 67) (ValInt 420)
          evalKvGet (ValInt 67)
        result @?= Right (ValInt 420),
      ---
      testCase "IO overwrite an existing key" $ do
        result <- runEvalIO $ do
          evalKvPut (ValInt 67) (ValInt 420)
          evalKvPut (ValInt 67) (ValBool False)
          evalKvGet (ValInt 67)
        result @?= Right (ValBool False),
      -- Step 7 behavior
      testCase "IO missing key" $ do
        result <- runEvalIO (evalKvGet (ValInt 99))
        result @?= Left "Non-existing key: ValInt 99",
      -- Task 4:
      testCase "Break outside loop (IO)" $ do
        (out, res) <- captureIO [] $ evalIO' (Break (CstBool True))
        (out, res) @?= ([], Left "Break outside loop"),
      --
      testCase "Break stops loop immediately (IO)" $ do
        (out, res) <-
          captureIO [] $
            evalIO' $
              ForLoop
                ("p", CstInt 0)
                ("i", CstInt 100)
                (Let "_" (Break (CstBool True)) (Var "i"))
        (out, res) @?= ([], Right (ValBool True))
    ]
