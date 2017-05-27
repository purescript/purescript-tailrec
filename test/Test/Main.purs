module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Rec.Class (Step(..), tailRec, tailRecM, tailRecM2)

import Data.Either (Either(..))

-- | Compute the nth triangle number
triangle :: Int -> Eff (console :: CONSOLE) Int
triangle = tailRecM2 f 0
  where
  f acc 0 = pure (Done acc)
  f acc n = do
    log $ "Accumulator: " <> show acc
    pure (Loop { a: acc + n, b: n - 1 })

loop :: Int -> Eff (console :: CONSOLE) Unit
loop n = tailRecM go n
  where
  go 0 = do
    log "Done!"
    pure (Done unit)
  go n' = pure (Loop (n' - 1))

loopFunction :: Int -> ({result :: Int, step :: Int} -> Int)
loopFunction = tailRecM go
  where
  go 0 = Done <$> \e -> e.result
  go n = Loop <$> \e -> n - e.step

mutual :: Int -> Boolean
mutual = tailRec go <<< Left
  where
  go (Left n) = even n
  go (Right n) = odd n

  even 0 = Done true
  even n = Loop (Right (n - 1))

  odd 0 = Done false
  odd n = Loop (Left (n - 1))

main :: Eff (console :: CONSOLE) Unit
main = do
  _ <- triangle 10
  logShow $ mutual 1000001
  loop 1000000
  logShow $ loopFunction 10000000 ({result:100, step:1})
