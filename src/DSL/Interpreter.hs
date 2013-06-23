{-# language
   ScopedTypeVariables
 , TypeFamilies
 , FlexibleContexts
 , NoMonomorphismRestriction
 #-}

module DSL.Interpreter
  ( interp
  , runIOBF
  , runDetBF
  )
where

import Control.Monad.State
import Control.Monad.Free
import Control.Arrow
import Control.Applicative
import Control.Monad.InOutMonad

import qualified Data.Map as Map
import Data.Char

import DSL.AST.Base

type Pointer = Int
type Memory  = Map.Map Pointer Char

type BFM m  = StateT (Pointer, Memory) m

getPtr :: (Functor m, Monad m) => BFM m Pointer
getPtr      = fst <$> get
modifyMem :: (Functor m, Monad m) => (Memory -> Memory) -> BFM m ()
modifyMem f = modify (second f)
modifyPtr :: (Functor m, Monad m) => (Pointer -> Pointer) -> BFM m ()
modifyPtr f = modify (first f)

getVal :: (Functor m, Monad m) => BFM m Char
getVal = do
  (p,m) <- get
  let val = Map.lookup p m
  return $ maybe (chr 0) id val

putVal :: (Functor m, Monad m) => Char -> BFM m ()
putVal c = do
  p <- getPtr
  modifyMem (Map.insert p c)

validate :: Int -> Int
validate i | 0 <= i && i <= 255 = i
validate i = error $ "Invalid char " ++ show i

interp :: forall m . ( Functor m, InOutMonad m
                     , I m ~ Char
                     , O m ~ Char)
       => AST () -> BFM m ()
interp = foldFree (const $ return ()) interp'
  where
    interp' :: (Functor m, InOutMonad m) => ASTF (BFM m ()) -> BFM m ()
    interp' prog = case prog of
      Switch i r -> do
        p <- getPtr
        when (p+i < 0) $
          error $ "(Switch " ++ show i ++ ") at " ++ show p
        modifyPtr (+i)
        r
      Arith op r -> do
        val <- getVal
        let oper = case op of
              Inc -> (+)
              Dec -> (-)
            val' = chr $ validate (ord val `oper` 1)
        putVal val'
        r
      InOut Put r -> do
        val <- getVal
        produceOutput val
        r
      InOut Get r -> do
        val <- consumeInput
        putVal val
        r
      While act r -> do
        let w = do
              val <- getVal
              when (ord val /= 0) (act >> w)
        w
        r
      Stop -> return ()

runIOBF :: BFM IO () -> IO ()
runIOBF bfm = evalStateT bfm (0,Map.empty)

runDetBF :: BFM (DetIO Char Char) () -> String -> String
runDetBF bfm input = flip execDetIO input $
                      evalStateT bfm $
                      (0,Map.empty)