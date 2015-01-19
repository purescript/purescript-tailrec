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

loop :: Number -> Eff (trace :: Trace) Unit
loop n = tailRecM go n
  where
  go 0 = do
    trace "Done!"
    return (Right unit)
  go n = return (Left (n - 1))
  
loopWriter :: Number -> WriterT Sum (Eff (trace :: Trace)) Unit
loopWriter n = tailRecM go n
  where
  go 0 = do
    lift $ trace "Done!"
    return (Right unit)
  go n = do
    tell $ Sum n  
    return (Left (n - 1))
    
loopError :: Number -> ErrorT String (Eff (trace :: Trace)) Unit
loopError n = tailRecM go n
  where
  go 0 = do
    throwError "Done!"
  go n = return (Left (n - 1))
    
loopState :: Number -> StateT Number (Eff (trace :: Trace)) Unit
loopState n = tailRecM go n
  where
  go 0 = do
    n <- get
    lift $ trace "Done!"
    return unit
  go n = do
    modify \s -> s + n 
    return (Left (n - 1))
  
main = do
  loop 1000000
  result1 <- runWriterT $ loopWriter 1000000
  print result1
  result2 <- runErrorT $ loopError 1000000
  print result2
  result3 <- runStateT (loopState 1000000) 0
  print result3
