module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    -- Task1
    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      let (ps, res) = runEval' r s (m1 >>= k)
      in case res of
        Right x -> (ps, Right x)
        Left _  -> runEval' r s (m2 >>= k)
    -- Task 3
    runEval' r s (Free (TransactionOp m k)) =
      case runEval' r s m of 
        (out, Left e) -> (out, Left e)
        (out, Right (v, s')) ->
          let (out', res) = runEval' r s' (k v)
            in (out ++ out', res)

    -- Task4   
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' _ _ (Free (BreakOp _)) = ([], Left "Break outside loop")
    runEval' r s (Free (LoopOp m k)) = 
      case runLoopBody r s m of 
        (out, Left e) -> (out, Left e)
        (out, Right v) ->
          let (out', res) = runEval' r s (k v)
            in (out ++ out', res)
    runLoopBody :: Env -> State -> EvalM Val -> ([String], Either Error Val)
    runLoopBody _ _ (Pure x) = ([], pure x)
    runLoopBody r s (Free (ReadOp k)) = runLoopBody r s $ k r
    runLoopBody r s (Free (PrintOp p m)) =
      let (ps, res) = runLoopBody r s m
       in (p : ps, res)
     
    runLoopBody _ _ (Free (ErrorOp e)) = ([], Left e)
    runLoopBody _ _ (Free (BreakOp v)) = ([], Right v)
    runLoopBody r s (Free (LoopOp m k)) = 
      case runLoopBody r s m of 
        (out, Left e) -> (out, Left e)
        (out, Right v) ->
          let (out', res) = runLoopBody r s (k v)
            in (out ++ out', res)
