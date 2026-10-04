module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    -- Task 1
    runEvalIO' r db (Free (TryCatchOp m1 m2 k)) = do
      res1 <- runEvalIO' r db m1
      case res1 of
        Right v -> runEvalIO' r db (k v)
        Left _ -> do
          res2 <- runEvalIO' r db m2
          case res2 of
            Right v -> runEvalIO' r db (k v)
            Left e -> pure $ Left e
    -- Task 3
    runEvalIO' r db (Free (TransactionOp m k)) = do
      res <- withTempDB $ \tmp -> do
        copyDB db tmp 
        res' <- runEvalIO' r tmp m 
        case res' of
          Left e -> pure $ Left e 
          Right v -> do 
            copyDB tmp db 
            pure $ Right v 
      case res of 
        Left e -> pure $ Left e 
        Right v -> runEvalIO' r db (k v)
    -- Task 4
    runEvalIO' _ _ (Free (BreakOp e)) = pure $ Left "Break outside loop"
    runEvalIO' r db (Free (LoopOp m k)) = do
      res <- runLoopBodyIO r db m
      case res of 
        Left e -> pure $ Left e 
        Right v -> runEvalIO' r db (k v)
    
    -- EvalM a is changed to EvalM Val in the following function because breakOp v always carries a concrete Val.
    runLoopBodyIO :: Env -> FilePath -> EvalM Val -> IO (Either Error Val)
    runLoopBodyIO _ _ (Pure x) = pure $ pure x
    runLoopBodyIO r db (Free (ReadOp k)) = runLoopBodyIO r db $ k r
    runLoopBodyIO r db (Free (PrintOp p m)) = do
      putStrLn p
      runLoopBodyIO r db m
    runLoopBodyIO _ _ (Free (ErrorOp e)) = pure $ Left e
    -- Task 4
    runLoopBodyIO _ _ (Free (BreakOp v)) = pure $ Right v
    runLoopBodyIO r db (Free (LoopOp m k)) = do
      res <- runLoopBodyIO r db m
      case res of 
        Left e -> pure $ Left e 
        Right v -> runLoopBodyIO r db (k v)
    -- Task 3
    runLoopBodyIO r db (Free (TransactionOp m k)) = do
      res <- withTempDB $ \tmp -> do
        copyDB db tmp 
        res' <- runLoopBodyIO r tmp m 
        case res' of
          Left e -> pure $ Left e 
          Right v -> do 
            copyDB tmp db 
            pure $ Right v 
      case res of 
        Left e -> pure $ Left e 
        Right v -> runLoopBodyIO r db (k v)


