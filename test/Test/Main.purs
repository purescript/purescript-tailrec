module Test.Main where

import Prelude
import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log, print)
import Control.Monad.Rec.Class
import Data.Either (Either(..))

-- | Compute the nth triangle number
triangle :: Int -> Eff (console :: CONSOLE) Int
triangle = tailRecM2 f 0
  where
  f acc 0 = return (Right acc)
  f acc n = do
    log $ "Accumulator: " <> show acc
    return (Left { a: acc + n, b: n - 1 })

loop :: Int -> Eff (console :: CONSOLE) Unit
loop n = tailRecM go n
  where
  go 0 = do
    log "Done!"
    return (Right unit)
  go n = return (Left (n - 1))

mutual :: Int -> Boolean
mutual = tailRec go <<< Left
  where
  go (Left n) = even n
  go (Right n) = odd n

  even 0 = Right true
  even n = Left (Right (n - 1))

  odd 0 = Right false
  odd n = Left (Left (n - 1))

main :: Eff (console :: CONSOLE) Unit
main = do
  triangle 10
  print $ mutual 1000001
  loop 1000000
