# Module Documentation

## Module Control.Monad.Rec.Class

### Types

#### `Step`

The result of a computation: either `Loop` containing the updated accumulator,
or `Done` containing the final result of the computation.

    data Step a b
      = Loop a
      | Done b


### Type Classes

#### `MonadRec`

This type class captures those monads which support tail recursion in constant stack space.

The `tailRecM` function takes a step function, and applies that step function recursively
until a return value of type `b` is found.

Instances are provided for standard monad transformers.

For example:

```purescript
loopWriter :: Number -> WriterT Sum (Eff (trace :: Trace)) Unit
loopWriter n = tailRecM go n
  where
  go 0 = do
    lift $ trace "Done!"
    return (Done unit)
  go n = do
    tell $ Sum n  
    return (Loop (n - 1))
```

    class (Monad m) <= MonadRec m where
      tailRecM :: forall a b. (a -> m (Step a b)) -> a -> m b


### Type Class Instances

#### `monadRecEff`

    instance monadRecEff :: MonadRec (Eff eff)

#### `monadRecErrorT`

    instance monadRecErrorT :: (Error e, MonadRec m) => MonadRec (ErrorT e m)

#### `monadRecIdentity`

    instance monadRecIdentity :: MonadRec Identity

#### `monadRecMaybeT`

    instance monadRecMaybeT :: (MonadRec m) => MonadRec (MaybeT m)

#### `monadRecStateT`

    instance monadRecStateT :: (MonadRec m) => MonadRec (StateT s m)

#### `monadRecWriterT`

    instance monadRecWriterT :: (Monoid w, MonadRec m) => MonadRec (WriterT w m)


### Values

#### `tailRec`

Create a pure tail-recursive function of one argument

For example:

```purescript
pow :: Number -> Number -> Number
pow n p = tailRec go { accum: 1, power: p }
  where
  go :: _ -> Step _ Number
  go { accum: acc, power: 0 } = Done acc
  go { accum: acc, power: p } = Loop { accum: acc * n, power: p - 1 }
```

    tailRec :: forall a b. (a -> Step a b) -> a -> b

#### `tailRecM2`

Create a tail-recursive function of two arguments which uses constant stack space.

    tailRecM2 :: forall m a b c. (MonadRec m) => (a -> b -> m (Step { b :: b, a :: a } c)) -> a -> b -> m c

#### `tailRecM3`

Create a tail-recursive function of three arguments which uses constant stack space.

    tailRecM3 :: forall m a b c d. (MonadRec m) => (a -> b -> c -> m (Step { c :: c, b :: b, a :: a } d)) -> a -> b -> c -> m d



