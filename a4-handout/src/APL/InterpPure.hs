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
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    -- Basically the same as A2, with the interpreter included this time
    runEval' r s (Free (KvGetOp key kVal)) =
      case lookup key s of
        Just value -> runEval' r s (kVal value)
        Nothing -> ([], Left ("Non-existing key: " ++ show key))
    runEval' r s (Free (KvPutOp key value next)) =
      let otherEntries = filter (\(storedKey, _) -> storedKey /= key) s
          newStore = (key, value) : otherEntries
      in runEval' r newStore next