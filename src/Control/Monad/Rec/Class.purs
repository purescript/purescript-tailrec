module Control.Monad.Rec.Class 
  ( MonadRec
  , Step(..)
  , tailRec
  , tailRecM
  , tailRecM2
  , tailRecM3
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


data Step a b = Loop a | Done b

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
-- |     return (Done unit)
-- |   go n = do
-- |     tell $ Sum n  
-- |     return (Loop (n - 1))
-- | ```
class (Monad m) <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Step a b)) -> a -> m b

-- | Create a tail-recursive function of two arguments which uses constant stack space.
tailRecM2 :: forall m a b c. (MonadRec m) => (a -> b -> m (Step { a :: a, b :: b } c)) -> a -> b -> m c
tailRecM2 f a b = tailRecM (\o -> f o.a o.b) { a: a, b: b }

-- | Create a tail-recursive function of three arguments which uses constant stack space.
tailRecM3 :: forall m a b c d. (MonadRec m) => (a -> b -> c -> m (Step { a :: a, b :: b, c :: c } d)) -> a -> b -> c -> m d
tailRecM3 f a b c = tailRecM (\o -> f o.a o.b o.c) { a: a, b: b, c: c }

-- | Create a pure tail-recursive function of one argument
-- |
-- | For example:
-- |
-- | ```purescript
-- | pow :: Number -> Number -> Number
-- | pow n p = tailRec go { accum: 1, power: p }
-- |   where
-- |   go :: _ -> Step _ Number
-- |   go { accum: acc, power: 0 } = Done acc
-- |   go { accum: acc, power: p } = Loop { accum: acc * n, power: p - 1 }
-- | ```
tailRec :: forall a b. (a -> Step a b) -> a -> b
tailRec f a = go (f a)
  where
  go (Loop a) = go (f a)
  go (Done b) = b

isLoop :: forall a b. Step a b -> Boolean
isLoop (Loop _) = true
isLoop _ = false

instance monadRecIdentity :: MonadRec Identity where
  tailRecM f = Identity <<< tailRec (runIdentity <<< f)

foreign import tailRecEff
  "function tailRecEff(f, br, loop, done, a) {\
  \  return function() {\
  \    var r = f(a)();\
  \    while (true) {\
  \      if (br(r)) {\
  \        var a1 = loop(r);\
  \        r = f(a1)();\
  \      } else {\
  \        return done(r);\
  \      }\
  \    }\
  \  };\
  \}" :: forall a b r e. Fn5 (a -> Eff e r) (r -> Boolean) (r -> a) (r -> b) a (Eff e b)

instance monadRecEff :: MonadRec (Eff eff) where
  tailRecM f a = runFn5 tailRecEff f isLoop fromLoop fromDone a
    where
    fromLoop (Loop a) = a
    fromDone (Done b) = b

instance monadRecMaybeT :: (MonadRec m) => MonadRec (MaybeT m) where
  tailRecM f = MaybeT <<< tailRecM \a -> do
    m <- runMaybeT (f a)
    return case m of
      Nothing -> Done Nothing
      Just (Loop a) -> Loop a
      Just (Done b) -> Done (Just b) 

instance monadRecErrorT :: (Error e, MonadRec m) => MonadRec (ErrorT e m) where
  tailRecM f = ErrorT <<< tailRecM \a -> do
    m <- runErrorT (f a)
    return case m of
      Left e -> Done (Left e)
      Right (Loop a) -> Loop a
      Right (Done b) -> Done (Right b) 

instance monadRecWriterT :: (Monoid w, MonadRec m) => MonadRec (WriterT w m) where
  tailRecM f a = WriterT $ tailRecM (\(Tuple a w) -> do
    Tuple m w1 <- runWriterT (f a)
    return case m of
      Loop a -> Loop (Tuple a (w <> w1))
      Done b -> Done (Tuple b (w <> w1))) (Tuple a mempty)

instance monadRecStateT :: (MonadRec m) => MonadRec (StateT s m) where
  tailRecM f a = StateT \s -> (tailRecM \(Tuple a s) -> do
    Tuple m s1 <- runStateT (f a) s
    return case m of
      Loop a -> Loop (Tuple a s1)
      Done b -> Done (Tuple b s1)) (Tuple a s)
      
