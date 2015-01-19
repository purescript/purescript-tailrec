# purescript-tailrec

A type class which captures stack-safe monadic tail recursion.

## Installing

```
bower i purescript-tailrec
```

## Usage

The PureScript compiler performs tail-call elimination for self-recursive functions, so that a function like

```purescript
pow :: Number -> Number -> Number
pow n p = go { accum: 1, power: p }
  where
  go { accum: acc, power: 0 } = acc
  go { accum: acc, power: p } = go { accum: acc * n, power: p - 1 }
```

gets compiled into an efficient `while` loop.

However, we do not get the same benefit when using monadic recursion:

```purescript
powWriter :: Number -> Number -> Writer Product Unit
powWriter n = go
  where
  go 0 = return unit
  go m = do
    tell n
    go (m - 1)
```

However, we can refactor the original function to isolate the recursive function call:

```purescript
pow :: Number -> Number -> Number
pow n p = tailRec go { accum: 1, power: p }
  where
  go :: _ -> Either _ Number
  go { accum: acc, power: 0 } = Right acc
  go { accum: acc, power: p } = Left { accum: acc * n, power: p - 1 }
```

where the `tailRec` function is defined in the `Control.Monad.Rec.Class` module, with type:

```purescript
tailRec :: forall a b. (a -> Either a b) -> a -> b
```

In the body of the loop, instead of calling the `go` function recursively, we return a value using the `Right` constructor. To break from the loop, we use the `Left` constructor.

This pattern can be generalized to several monad transformers from the `purescript-transformers` library using the following type class:

```purescript
class (Monad m) <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Either a b)) -> a -> m b
```

This library provides stack-safe implementations of this class for several monads, so that we can rewrite `powWriter` as follows, without worrying about blowing the stack:

```purescript
powWriter :: Number -> Number -> Writer Product Unit
powWriter n = tailRecM go
  where
  go 0 = return (Left unit)
  go m = do
    tell n
    return $ Right (m - 1)
```
