# Module Documentation

## Module Control.Monad.Rec.Class

### Type Classes

#### `MonadRec`

    class (Monad m) <= MonadRec m where
      tailRecM :: forall a b. (a -> m (Either a b)) -> a -> m b


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

    tailRec :: forall a b. (a -> Either a b) -> a -> b

#### `tailRecM2`

    tailRecM2 :: forall m a b c. (MonadRec m) => (a -> b -> m (Either { b :: b, a :: a } c)) -> a -> b -> m c

#### `tailRecM3`

    tailRecM3 :: forall m a b c d. (MonadRec m) => (a -> b -> c -> m (Either { c :: c, b :: b, a :: a } d)) -> a -> b -> c -> m d



