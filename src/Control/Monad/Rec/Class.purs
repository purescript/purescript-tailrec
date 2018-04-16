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

import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect (Effect, untilE)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)

-- | The result of a computation: either `Loop` containing the updated
-- | accumulator, or `Done` containing the final result of the computation.
data Step a b = Loop a | Done b

derive instance functorStep :: Functor (Step a)

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

instance monadRecEffect :: MonadRec Effect where
  tailRecM f a = do
    r <- Ref.new =<< f a
    untilE do
      Ref.read r >>= case _ of
        Loop a' -> do
          e <- f a'
          _ <- Ref.write e r
          pure false
        Done b -> pure true
    fromDone <$> Ref.read r
    where
    fromDone :: forall a b. Step a b -> b
    fromDone = unsafePartial \(Done b) -> b

instance monadRecFunction :: MonadRec ((->) e) where
  tailRecM f a0 e = tailRec (\a -> f a e) a0

instance monadRecEither :: MonadRec (Either e) where
  tailRecM f a0 =
    let
      g (Left e) = Done (Left e)
      g (Right (Loop a)) = Loop (f a)
      g (Right (Done b)) = Done (Right b)
    in tailRec g (f a0)

instance monadRecMaybe :: MonadRec Maybe where
  tailRecM f a0 =
    let
      g Nothing = Done Nothing
      g (Just (Loop a)) = Loop (f a)
      g (Just (Done b)) = Done (Just b)
    in tailRec g (f a0)

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
