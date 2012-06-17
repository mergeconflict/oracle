{-# LANGUAGE DoRec, GeneralizedNewtypeDeriving #-}

module Control.Monad.State.Recursive
       ( RecStateT
       , tie
       , past
       , present
       , future
       , evalRecStateT
       , execRecStateT
       ) where

import           Control.Applicative (Alternative, Applicative, (<$>))
import           Control.Monad (MonadPlus)
import           Control.Monad.Cont (MonadCont)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Writer (MonadWriter)
import           Control.Monad.Error (MonadError)
import           Control.Monad.Trans (MonadTrans)
import qualified Control.Monad.State as S

newtype RecStateT s m a = RecStateT (S.StateT (s, s) m a) deriving
  ( Alternative
  , Applicative
  , Functor
  , Monad
  , MonadCont
  , MonadError e
  , MonadFix
  , MonadIO
  , MonadPlus
  , MonadReader r
  , MonadTrans
  , MonadWriter w )

tie :: MonadFix m => RecStateT s m a -> s -> m (a, s)
tie (RecStateT m) s = do
  rec (a, (s', _)) <- S.runStateT m (s, s')
  return (a, s')

past :: (Functor m, Monad m) => RecStateT s m s
past = fst <$> RecStateT S.get

present :: Monad m => s -> RecStateT s m ()
present s = RecStateT $ S.modify $ \ (_, s') -> (s, s')

future :: (Functor m, Monad m) => RecStateT s m s
future = snd <$> RecStateT S.get

evalRecStateT :: (Functor m, MonadFix m) => RecStateT s m a -> s -> m a
evalRecStateT m s = fst <$> tie m s

execRecStateT :: (Functor m, MonadFix m) => RecStateT s m a -> s -> m s
execRecStateT m s = snd <$> tie m s
