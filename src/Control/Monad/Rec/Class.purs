module Control.Monad.Rec.Class 
  ( MonadRec
  , tailRec
  , tailRecM
  , tailRecM2
  , tailRecM3
  , forever
  ) where

import Data.Maybe
import Data.Either
import Data.Function
import Data.Monoid
import Data.Tuple
import Data.Identity

import Control.Functor ((<$))

import Control.Monad.Eff
import Control.Monad.ST

import Control.Monad.Maybe.Trans
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.State.Trans
import Control.Monad.Writer.Trans

-- | This type class captures those monads which support tail recursion in constant stack space.
-- |
-- | The `tailRecM` function takes a step function, and applies that step function recursively
-- | until a return value of type `b` is found.
-- |
-- | Instances are provided for standard monad transformers.
-- |
-- | For example:
-- |
-- | ```purescript
-- | loopWriter :: Number -> WriterT Sum (Eff (trace :: Trace)) Unit
-- | loopWriter n = tailRecM go n
-- |   where
-- |   go 0 = do
-- |     lift $ trace "Done!"
-- |     return (Right unit)
-- |   go n = do
-- |     tell $ Sum n  
-- |     return (Left (n - 1))
-- | ```
class (Monad m) <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Either a b)) -> a -> m b

-- | Create a tail-recursive function of two arguments which uses constant stack space.
tailRecM2 :: forall m a b c. (MonadRec m) => (a -> b -> m (Either { a :: a, b :: b } c)) -> a -> b -> m c
tailRecM2 f a b = tailRecM (\o -> f o.a o.b) { a: a, b: b }

-- | Create a tail-recursive function of three arguments which uses constant stack space.
tailRecM3 :: forall m a b c d. (MonadRec m) => (a -> b -> c -> m (Either { a :: a, b :: b, c :: c } d)) -> a -> b -> c -> m d
tailRecM3 f a b c = tailRecM (\o -> f o.a o.b o.c) { a: a, b: b, c: c }

-- | Create a pure tail-recursive function of one argument
-- |
-- | For example:
-- |
-- | ```purescript
-- | pow :: Number -> Number -> Number
-- | pow n p = tailRec go { accum: 1, power: p }
-- |   where
-- |   go :: _ -> Either _ Number
-- |   go { accum: acc, power: 0 } = Right acc
-- |   go { accum: acc, power: p } = Left { accum: acc * n, power: p - 1 }
-- | ```
tailRec :: forall a b. (a -> Either a b) -> a -> b
tailRec f a = go (f a)
  where
  go (Left a) = go (f a)
  go (Right b) = b

instance monadRecIdentity :: MonadRec Identity where
  tailRecM f = Identity <<< tailRec (runIdentity <<< f)

tailRecEff :: forall a b eff. (a -> Eff eff (Either a b)) -> a -> Eff eff b
tailRecEff f a = runST do
  e <- f' a
  r <- newSTRef e
  untilE do
    e <- readSTRef r
    case e of
      Left a' -> do e' <- f' a'
                    writeSTRef r e'
                    return false
      Right b -> return true
  fromRight <$> readSTRef r
  where
  f' :: forall h. a -> Eff (st :: ST h | eff) _
  f' = Control.Monad.Eff.Unsafe.unsafeInterleaveEff <<< f

  fromRight :: forall a b. Either a b -> b
  fromRight (Right b) = b

instance monadRecEff :: MonadRec (Eff eff) where
  tailRecM = tailRecEff

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
  tailRecM f a = WriterT $ tailRecM f' (Tuple a mempty)
    where
    f' (Tuple a w) = do
      Tuple m w1 <- runWriterT (f a)
      return case m of
        Left a -> Left (Tuple a (w <> w1))
        Right b -> Right (Tuple b (w <> w1))

instance monadRecStateT :: (MonadRec m) => MonadRec (StateT s m) where
  tailRecM f a = StateT \s -> tailRecM f' (Tuple a s)
    where
    f' (Tuple a s) = do
      Tuple m s1 <- runStateT (f a) s
      return case m of
        Left a -> Left (Tuple a s1)
        Right b -> Right (Tuple b s1)

-- | `forever` runs an action indefinitely, using the `MonadRec` instance to
-- | ensure constant stack usage.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = forever $ trace "Hello, World!"
-- | ```
forever :: forall m a b. (MonadRec m) => m a -> m b
forever ma = tailRecM (\u -> Left u <$ ma) unit
