module Control.Monad.Rec.Class
  ( Step(..)
  , class MonadRec
  , tailRec
  , tailRecM
  , tailRecM2
  , tailRecM3
  , forever
  ) where

import Prelude

import Control.Monad.Eff (Eff, untilE)
import Control.Monad.Eff.Unsafe as U
import Control.Monad.ST (ST, runST, newSTRef, readSTRef, writeSTRef)

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Bifunctor (class Bifunctor)

import Partial.Unsafe (unsafePartial)

-- | The result of a computation: either `Loop` containing the updated
-- | accumulator, or `Done` containing the final result of the computation.
data Step a b = Loop a | Done b

instance functorStep :: Functor (Step a) where
  map f (Loop a) = Loop a
  map f (Done b) = Done (f b)

instance bifunctorStep :: Bifunctor Step where
  bimap f _ (Loop a) = Loop (f a)
  bimap _ g (Done b) = Done (g b)

-- | This type class captures those monads which support tail recursion in
-- | constant stack space.
-- |
-- | The `tailRecM` function takes a step function, and applies that step
-- | function recursively until a pure value of type `b` is found.
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
-- |     pure (Done unit)
-- |   go n = do
-- |     tell $ Sum n
-- |     pure (Loop (n - 1))
-- | ```
class Monad m <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Step a b)) -> a -> m b

-- | Create a tail-recursive function of two arguments which uses constant stack space.
tailRecM2
  :: forall m a b c
   . MonadRec m
  => (a -> b -> m (Step { a :: a, b :: b } c))
  -> a
  -> b
  -> m c
tailRecM2 f a b = tailRecM (\o -> f o.a o.b) { a, b }

-- | Create a tail-recursive function of three arguments which uses constant stack space.
tailRecM3
  :: forall m a b c d
   . MonadRec m
  => (a -> b -> c -> m (Step { a :: a, b :: b, c :: c } d))
  -> a
  -> b
  -> c
  -> m d
tailRecM3 f a b c = tailRecM (\o -> f o.a o.b o.c) { a, b, c }

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
tailRec f = go <<< f
  where
  go (Loop a) = go (f a)
  go (Done b) = b

instance monadRecIdentity :: MonadRec Identity where
  tailRecM f = Identity <<< tailRec (runIdentity <<< f)
    where runIdentity (Identity x) = x

instance monadRecEff :: MonadRec (Eff eff) where
  tailRecM = tailRecEff

instance monadRecEither :: MonadRec (Either e) where
  tailRecM f a0 =
    let
      g (Left e) = Done (Left e)
      g (Right (Loop a)) = Loop (f a)
      g (Right (Done b)) = Done (Right b)
    in tailRec g (f a0)

tailRecEff :: forall a b eff. (a -> Eff eff (Step a b)) -> a -> Eff eff b
tailRecEff f a = runST do
  e <- f' a
  r <- newSTRef e
  untilE do
    e' <- readSTRef r
    case e' of
      Loop a' -> do
        e'' <- f' a'
        writeSTRef r e''
        pure false
      Done b -> pure true
  fromDone <$> readSTRef r
  where
  f' :: forall h. a -> Eff (st :: ST h | eff) (Step a b)
  f' = U.unsafeCoerceEff <<< f
  fromDone :: Step a b -> b
  fromDone = unsafePartial \(Done b) -> b

-- | `forever` runs an action indefinitely, using the `MonadRec` instance to
-- | ensure constant stack usage.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = forever $ trace "Hello, World!"
-- | ```
forever :: forall m a b. MonadRec m => m a -> m b
forever ma = tailRecM (\u -> Loop u <$ ma) unit
