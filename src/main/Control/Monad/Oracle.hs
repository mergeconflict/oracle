{-# LANGUAGE DoRec, GeneralizedNewtypeDeriving #-}

module Control.Monad.Oracle
       ( OracleT
       , evalOracleT
       , execOracleT
       , runOracleT
       ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.RWS

newtype OracleT o s m a = OracleT (RWST o o s m a) deriving
  ( Alternative
  , Applicative
  , Functor
  , Monad
  , MonadCont
  , MonadError e
  , MonadFix
  , MonadIO
  , MonadPlus
  , MonadReader o
  , MonadWriter o
  , MonadState s
  , MonadRWS o o s
  , MonadTrans )

evalOracleT :: (Functor m, MonadFix m) => OracleT o s m a -> s -> m (a, o)
evalOracleT m s =
  let discardSnd (a, _, c) = (a, c)
  in discardSnd <$> runOracleT m s

execOracleT :: (Functor m, MonadFix m) => OracleT o s m a -> s -> m (s, o)
execOracleT m s =
  let discardFst (_, b, c) = (b, c)
  in discardFst <$> runOracleT m s

runOracleT :: MonadFix m => OracleT o s m a -> s -> m (a, s, o)
runOracleT (OracleT rwst) s = do
  rec (a, s', o) <- runRWST rwst o s
  return (a, s', o)
