{-# language
   TypeFamilies
 , GeneralizedNewtypeDeriving
 #-}

module Control.Monad.InOutMonad where

import Control.Monad.State
import Control.Monad.Writer

class Monad m => InOutMonad m where
  type I m :: *
  type O m :: *
  consumeInput  :: m (I m)
  produceOutput :: O m -> m ()

newtype DetIO i o a = DetIO { runDetIO :: StateT [i] (Writer [o]) a }
                   deriving (Monad, Functor)

execDetIO :: DetIO i o a -> [i] -> [o]
execDetIO (DetIO st) input = snd $ runWriter $ evalStateT st input

instance InOutMonad m => InOutMonad (StateT s m) where
  type I (StateT s m) = I m
  type O (StateT s m) = O m
  consumeInput  = lift consumeInput
  produceOutput = lift . produceOutput

instance InOutMonad (DetIO i o) where
  type I (DetIO i o) = i
  type O (DetIO i o) = o
  
  consumeInput = DetIO $ do
    i <- gets head
    modify tail
    return i

  produceOutput = DetIO . tell . return

instance InOutMonad IO where
  type I IO = Char
  type O IO = Char
  consumeInput  = getChar
  produceOutput = putChar
