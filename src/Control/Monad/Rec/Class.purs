module Control.Monad.Rec.Class 
  ( MonadRec
  , tailRec
  , tailRecM
  ) where

import Data.Maybe
import Data.Either
import Data.Function
import Data.Monoid
import Data.Tuple
import Data.Identity

import Control.Monad.Eff
import Control.Monad.Maybe.Trans
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.State.Trans
import Control.Monad.Writer.Trans

class (Monad m) <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Either a b)) -> a -> m b

tailRec :: forall a b. (a -> Either a b) -> a -> b
tailRec f a = go (f a)
  where
  go (Left a) = go (f a)
  go (Right b) = b

instance monadRecIdentity :: MonadRec Identity where
  tailRecM f = Identity <<< tailRec (runIdentity <<< f)

foreign import tailRecEff
  "function tailRecEff(f, br, left, right, a) {\
  \  return function() {\
  \    var r = f(a)();\
  \    while (true) {\
  \      if (br(r)) {\
  \        var a1 = left(r);\
  \        r = f(a1)();\
  \      } else {\
  \        return right(r);\
  \      }\
  \    }\
  \  };\
  \}" :: forall a b r e. Fn5 (a -> Eff e r) (r -> Boolean) (r -> a) (r -> b) a (Eff e b)

instance monadRecEff :: MonadRec (Eff eff) where
  tailRecM f a = runFn5 tailRecEff f isLeft fromLeft fromRight a
    where
    fromLeft (Left a) = a
    fromRight (Right b) = b

instance monadRecMaybeT :: (MonadRec m) => MonadRec (MaybeT m) where
  tailRecM f = MaybeT <<< tailRecM \a -> do
    m <- runMaybeT (f a)
    return case m of
      Nothing -> Right Nothing
      Just (Left a) -> Left a
      Just (Right b) -> Right (Just b) 

instance monadRecErrorT :: (Error e, MonadRec m) => MonadRec (ErrorT e m) where
  tailRecM f = ErrorT <<< tailRecM \a -> do
    m <- runErrorT (f a)
    return case m of
      Left e -> Right (Left e)
      Right (Left a) -> Left a
      Right (Right b) -> Right (Right b) 

instance monadRecWriterT :: (Monoid w, MonadRec m) => MonadRec (WriterT w m) where
  tailRecM f a = WriterT $ tailRecM (\(Tuple a w) -> do
    Tuple m w1 <- runWriterT (f a)
    return case m of
      Left a -> Left (Tuple a (w <> w1))
      Right b -> Right (Tuple b (w <> w1))) (Tuple a mempty)

instance monadRecStateT :: (MonadRec m) => MonadRec (StateT s m) where
  tailRecM f a = StateT \s -> (tailRecM \(Tuple a s) -> do
    Tuple m s1 <- runStateT (f a) s
    return case m of
      Left a -> Left (Tuple a s1)
      Right b -> Right (Tuple b s1)) (Tuple a s)
      