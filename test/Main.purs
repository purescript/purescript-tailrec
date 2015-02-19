module Test.Main where

import Debug.Trace

import Data.Maybe
import Data.Either
import Data.Function
import Data.Monoid
import Data.Monoid.Sum
import Data.Tuple
import Data.Identity

import Control.Monad.Eff
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Error.Class
import Control.Monad.Writer.Trans
import Control.Monad.Writer.Class
import Control.Monad.State.Trans
import Control.Monad.State.Class

import Control.Monad.Rec.Class 

-- | Compute the nth triangle number
triangle :: Number -> Eff (trace :: Trace) Number
triangle = tailRecM2 f 0
  where
  f acc 0 = return (Done acc)
  f acc n = do
    trace $ "Accumulator: " <> show acc
    return (Loop { a: acc + n, b: n - 1 })

loop :: Number -> Eff (trace :: Trace) Unit
loop n = tailRecM go n
  where
  go 0 = do
    trace "Done!"
    return (Done unit)
  go n = return (Loop (n - 1))
  
loopWriter :: Number -> WriterT Sum (Eff (trace :: Trace)) Unit
loopWriter n = tailRecM go n
  where
  go 0 = do
    lift $ trace "Done!"
    return (Done unit)
  go n = do
    tell $ Sum n  
    return (Loop (n - 1))
    
loopError :: Number -> ErrorT String (Eff (trace :: Trace)) Unit
loopError n = tailRecM go n
  where
  go 0 = do
    throwError "Done!"
  go n = return (Loop (n - 1))
    
loopState :: Number -> StateT Number (Eff (trace :: Trace)) Unit
loopState n = tailRecM go n
  where
  go 0 = do
    lift $ trace "Done!"
    return (Done unit)
  go n = do
    modify \s -> s + n 
    return (Loop (n - 1))
  
main = do
  triangle 10
  loop 1000000
  result1 <- runWriterT $ loopWriter 1000000
  print result1
  result2 <- runErrorT $ loopError 1000000
  print result2
  result3 <- runStateT (loopState 1000000) 0
  print result3
