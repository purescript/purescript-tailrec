# purescript-tailrec

[![Latest release](http://img.shields.io/github/release/purescript/purescript-tailrec.svg)](https://github.com/purescript/purescript-tailrec/releases)
[![Build status](https://travis-ci.org/purescript/purescript-tailrec.svg?branch=master)](https://travis-ci.org/purescript/purescript-tailrec)

A type class which captures stack-safe monadic tail recursion.

## Installation

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

In the body of the loop, instead of calling the `go` function recursively, we return a value using the `Left` constructor. To break from the loop, we use the `Right` constructor.

This pattern can be generalized to several monad transformers from the `purescript-transformers` library using the following type class:

```purescript
class Monad m <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Either a b)) -> a -> m b
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-tailrec).
