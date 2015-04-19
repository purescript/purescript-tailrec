module Test.Main where

import Console 

import Data.Maybe
import Data.Either
import Data.Function
import Data.Monoid
import Data.Monoid.Additive
import Data.Tuple
import Data.Identity

import Control.Monad (when)
import Control.Monad.Eff
import Control.Monad.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Error.Class
import Control.Monad.Writer.Trans
import Control.Monad.Writer.Class
import Control.Monad.State.Trans
import Control.Monad.State.Class

import Control.Monad.Rec.Class 

-- | Compute the nth triangle number
triangle :: Number -> Eff (console :: CONSOLE) Number
triangle = tailRecM2 f 0
  where
  f acc 0 = return (Right acc)
  f acc n = do
    log $ "Accumulator: " <> show acc
    return (Left { a: acc + n, b: n - 1 })

loop :: Number -> Eff (console :: CONSOLE) Unit
loop n = tailRecM go n
  where
  go 0 = do
    log "Done!"
    return (Right unit)
  go n = return (Left (n - 1))
  
loopWriter :: Number -> WriterT (Additive Number) (Eff (console :: CONSOLE)) Unit
loopWriter n = tailRecM go n
  where
  go 0 = do
    lift $ log "Done!"
    return (Right unit)
  go n = do
    tell $ Additive n  
    return (Left (n - 1))
    
loopError :: Number -> ErrorT String (Eff (console :: CONSOLE)) Unit
loopError n = tailRecM go n
  where
  go 0 = do
    throwError "Done!"
  go n = return (Left (n - 1))
    
loopState :: Number -> StateT Number (Eff (console :: CONSOLE)) Unit
loopState n = tailRecM go n
  where
  go 0 = do
    lift $ log "Done!"
    return (Right unit)
  go n = do
    modify \s -> s + n 
    return (Left (n - 1))
 
mutual :: Number -> Boolean
mutual = tailRec go <<< Left
  where
  go (Left n) = even n
  go (Right n) = odd n

  even 0 = Right true
  even n = Left (Right (n - 1))

  odd 0 = Right false
  odd n = Left (Left (n - 1))

main = do
  triangle 10
  print $ mutual 1000001
  loop 1000000
  result1 <- runWriterT $ loopWriter 1000000
  print result1
  result2 <- runErrorT $ loopError 1000000
  print result2
  result3 <- runStateT (loopState 1000000) 0
  print result3

  flip runStateT 0 $ runMaybeT $ forever do
    n <- get
    lift $ lift $ print n
    when (n == 10) $ MaybeT $ return Nothing
    put (n + 1)
