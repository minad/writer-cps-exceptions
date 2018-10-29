{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Trans.Writer.CPS.Exceptions where

import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Monad.Trans.Writer.CPS

instance (MonadThrow m, Monoid w) => MonadThrow (WriterT w m) where
  throwM e = lift $ throwM e

instance (MonadCatch m, Monoid w) => MonadCatch (WriterT w m) where
  catch m h = writerT $ runWriterT m `catch ` \e -> runWriterT (h e)

instance (MonadMask m, Monoid w) => MonadMask (WriterT w m) where
  mask a = writerT $ mask $ \u -> runWriterT (a $ q u)
    where q u b = writerT $ u (runWriterT b)
  uninterruptibleMask a =
    writerT $ uninterruptibleMask $ \u -> runWriterT (a $ q u)
      where q u b = writerT $ u (runWriterT b)
  generalBracket acquire release use = writerT $ do
    ((b, _w12), (c, w123)) <- generalBracket
      (runWriterT acquire)
      (\(resource, w1) exitCase -> case exitCase of
        ExitCaseSuccess (b, w12) -> do
          (c, w3) <- runWriterT (release resource (ExitCaseSuccess b))
          return (c, mappend w12 w3)
        ExitCaseException e -> do
          (c, w3) <- runWriterT (release resource (ExitCaseException e))
          return (c, mappend w1 w3)
        ExitCaseAbort -> do
          (c, w3) <- runWriterT (release resource ExitCaseAbort)
          return (c, mappend w1 w3))
      (\(resource, w1) -> do
        (a, w2) <- runWriterT (use resource)
        return (a, mappend w1 w2))
    return ((b, c), w123)
