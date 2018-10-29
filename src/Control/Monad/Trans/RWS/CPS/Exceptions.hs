{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Trans.RWS.CPS.Exceptions where

import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Monad.Trans.RWS.CPS

instance (MonadThrow m, Monoid w) => MonadThrow (RWST r w s m) where
  throwM e = lift $ throwM e
instance (MonadCatch m, Monoid w) => MonadCatch (RWST r w s m) where
  catch m h = rwsT $ \r s -> runRWST m r s `catch` \e -> runRWST (h e) r s
instance (MonadMask m, Monoid w) => MonadMask (RWST r w s m) where
  mask a = rwsT $ \r s -> mask $ \u -> runRWST (a $ q u) r s
    where q u b = rwsT $ \ r s -> u (runRWST b r s)
  uninterruptibleMask a =
    rwsT $ \r s -> uninterruptibleMask $ \u -> runRWST (a $ q u) r s
      where q u b = rwsT $ \ r s -> u (runRWST b r s)

  generalBracket acquire release use = rwsT $ \r s0 -> do
    ((b, _s2, _w12), (c, s3, w123)) <- generalBracket
      (runRWST acquire r s0)
      (\(resource, s1, w1) exitCase -> case exitCase of
        ExitCaseSuccess (b, s2, w12) -> do
          (c, s3, w3) <- runRWST (release resource (ExitCaseSuccess b)) r s2
          return (c, s3, mappend w12 w3)
        ExitCaseException e -> do
          (c, s3, w3) <- runRWST (release resource (ExitCaseException e)) r s1
          return (c, s3, mappend w1 w3)
        ExitCaseAbort -> do
          (c, s3, w3) <- runRWST (release resource ExitCaseAbort) r s1
          return (c, s3, mappend w1 w3))
      (\(resource, s1, w1) -> do
        (a, s2, w2) <- runRWST (use resource) r s1
        return (a, s2, mappend w1 w2))
    return ((b, c), s3, w123)
