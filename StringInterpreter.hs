{-# LANGUAGE
   ScopedTypeVariables #-}

module StringInterpreter where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Control.Arrow
import Control.Applicative hiding ((<|>),many)
import Data.Char
import Data.Functor
import Data.Function

import Debug.Trace

import FreeInterpreter
import qualified Types

---------------------------------
-- Real evaluators

type Prog    = Free BF ()
type Pointer = Int
type Memory  = Map.Map Pointer Char
type BFM     = StateT (String, (Pointer, Memory))
                (Writer String)

getPM = snd <$> get
getInput = fst <$> get
putInput x = modify (first $ const x)
modifyPM f = modify (second f)

---------------------------------
-- Helpers

validate :: Int -> Int
validate i | 0 <= i && i <= 255 = i
validate i = error $ "Invalid char " ++ show i

getVal = do
  (p,m) <- getPM
  let val = Map.lookup p m
  return $ maybe (chr 0) id val

putVal c = do
  (p,_) <- getPM
  modifyPM (second $ Map.insert p c)

interpUnfix :: (Free BF () -> BFM ()) ->
                Free BF () -> BFM ()
interpUnfix _loop (Open ()) = return ()

interpUnfix loop prog@(Free bf) = case bf of
  Succ r -> do
    modifyPM (first succ)
    loop r
  Pred r -> do
    (p,_) <- getPM
    when (p==0) (error "Pred for ptr==0")
    modifyPM (first pred)
    loop r
  Inc r -> do
    val <- getVal
    let val' = chr $ validate (ord val + 1)
    putVal val'
    loop r
  Dec r -> do
    val <- getVal
    let val' = chr $ validate (ord val - 1)
    putVal val'
    loop r
  PutChar r -> do
    val <- getVal
    tell [val]
    loop r
  GetChar r -> do
    ((val:rest)::String) <- getInput
    putInput rest
    putVal val
    loop r
  While bf r -> do
    val <- getVal
    if (ord val == 0)
      then
        loop r
      else do
        loop bf
        loop prog

----------------------------
-- Big fish

interp, interp' :: Free BF () -> BFM ()
interp  = fix  interpUnfix
interp' = fix (interpUnfix . hook)
  where
    hook loop x = do
      printState
      trace ("comm = " ++ prettyShallow x)
        loop x

    printState :: BFM ()
    printState = do
      (p,m) <- get
      trace ("pointer: " ++ show p ++
             " map: "    ++ show m)
        return ()

runBFM :: BFM () -> String -> String
runBFM bfm input = snd $ runWriter $
                   evalStateT bfm $
                   (input,(0,Map.empty))

commandToProg :: [Types.Command] -> Prog
commandToProg = mapM_ trans
  where
    trans :: Types.Command -> Prog
    trans Types.Succ = succ'
    trans Types.Pred = pred'
    trans Types.Inc = inc
    trans Types.Dec = dec
    trans Types.PutChar = putchar
    trans Types.GetChar = getchar
    trans (Types.While comms) = while (commandToProg comms)