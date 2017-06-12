module Data.Iterable 
  ( class Iterable
  , Iterator(..)
  , ArrayIndex
  , iterator
  , foreach
  , foldRec
  ) where

import Prelude
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM, tailRecM2)

class Iterable t s | t -> s where
  iterator :: forall a. t a -> Iterator s a

data Iterator s a = Iterator {
  state :: s a,
  get   :: s a -> a,
  next  :: s a -> (Step (s a) Unit)
}

foreach :: forall t s a m. Iterable t s => MonadRec m => (a -> m Unit) -> t a -> m Unit
foreach f = iterate f <<< iterator

foldRec :: forall t s a b m. Iterable t s => MonadRec m => (a -> b -> m b) -> b -> t a -> m b
foldRec f accum = inject f accum <<< iterator

iterate :: forall s a m. MonadRec m => (a -> m Unit) -> Iterator s a -> m Unit
iterate effect (Iterator { state: s, get: g, next: n}) = tailRecM go s
  where go s' = effect (g s') *> pure (n s')

inject :: forall s a b m. MonadRec m => (a -> b -> m b) -> b -> Iterator s a -> m b
inject effect accum (Iterator { state: s, get: g, next: n}) =
  tailRecM2 go s accum
  where
    go :: s a -> b -> m (Step { a :: s a, b :: b } b)
    go s' accum' = do
      accum'' <- effect (g s') accum'
      pure $ case n s' of
        Loop s''  -> Loop {a: s'', b: accum''}
        Done unit -> Done accum''

instance iterableArray :: Iterable Array ArrayIndex where
  iterator arr = Iterator {
    state: Index 0,
    get: indexArray arr,
    next: mkArrayNext (arrayLengthImpl arr - 1)
  }

newtype ArrayIndex a = Index Int

foreign import arrayIndexImpl :: forall a. Array a -> Int -> a
foreign import arrayLengthImpl :: forall a. Array a -> Int

indexArray :: forall a. Array a -> ArrayIndex a -> a
indexArray arr (Index i) = arrayIndexImpl arr i

mkArrayNext :: forall a. Int -> ArrayIndex a -> Step (ArrayIndex a) Unit
mkArrayNext lastIndex (Index i) =
  if i < lastIndex then Loop (Index (i + 1)) else Done unit
