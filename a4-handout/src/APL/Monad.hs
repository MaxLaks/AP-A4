module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    evalPrint,
    catch,
    failure,
    evalKvGet,
    evalKvPut,
    transaction,
    looping,
    breakLoop,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)
import GHC.OldList (zip4)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

-- k/v store
type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

data EvalOp a
  = ReadOp (Env -> a)
  | PrintOp String a
  | ErrorOp Error
  | KvGetOp Val (Val -> a)
  | KvPutOp Val Val a
  | TransactionOp (EvalM Val) (Val -> a)
  | BreakOp Val
  | LoopOp (EvalM Val) (Val -> a)
  | TryCatchOp (EvalM Val) (EvalM Val) (Val -> a)

instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (PrintOp p m) = PrintOp p $ f m
  fmap _ (ErrorOp e) = ErrorOp e
  fmap f (KvGetOp key x) = KvGetOp key $ f . x 
  fmap f (KvPutOp key value z) = KvPutOp key value (f z)

  fmap f (TransactionOp m k) = TransactionOp m $ f . k
  fmap _ (BreakOp v) = BreakOp v
  fmap f (LoopOp m k) = LoopOp m $ f . k
  fmap f (TryCatchOp m1 m2 k) = TryCatchOp m1 m2 $ f . k

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects ::
  (Functor e, Functor h) =>
  (e (Free e a) -> h (Free e a)) ->
  Free e a ->
  Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    -- TODO: add cases for TryCatchOp, TransactionOp, and as necessary for the
    -- effects you add for looping.
    g (TransactionOp m k) = TransactionOp (localEnv f m) k
    g (LoopOp m k) = LoopOp (localEnv f m) k
    g (TryCatchOp m1 m2 k) = TryCatchOp (localEnv f m1) (localEnv f m2) k
    g op = op

evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure ()

failure :: String -> EvalM a
failure = Free . ErrorOp

catch :: EvalM Val -> EvalM Val -> EvalM Val
catch = error "TODO"

evalKvGet :: Val -> EvalM Val
evalKvGet key = Free (KvGetOp key (\value -> pure value))     

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key value = Free (KvPutOp key value $ pure())

transaction :: EvalM Val -> EvalM Val
transaction m = Free $ TransactionOp m pure

-- | Enclose a computation @m@ such that if a 'breakLoop' is executed in @m@,
-- execution will return here.
looping :: EvalM Val -> EvalM Val
looping m = Free $ LoopOp m pure 

-- | Return the provided value from the most immediately enclosing 'looping'.
breakLoop :: Val -> EvalM a
breakLoop v = Free $ BreakOp v 
