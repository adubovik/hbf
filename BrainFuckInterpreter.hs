module BrainFuckInterpreter where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Arrow
import Control.Applicative hiding ((<|>),many)
import Data.Char
import Data.Functor

import Debug.Trace

import Types

type Prog = [Command]
type Pointer = Int
type Memory  = Map.Map Pointer Char
type BFM     = StateT (Pointer, Memory) IO

validate :: Int -> Int
validate i | 0 <= i && i < 255 = i
validate i = error $ "Invalid char " ++ show i

getVal = do
  (p,m) <- get
  let val = Map.lookup p m
  return $ maybe (chr 0) id val
putVal c = do
  (p,_) <- get
  modify (second $ Map.insert p c)

printState :: BFM ()
printState = do
  (p,m) <- get
  trace ("pointer: " ++ show p ++ " map: " ++ show m)
    return()

interpAtom' x = do
  printState
  trace ("comm = " ++ show x)
    interpAtom x

interpAtom :: Command ->  BFM ()
interpAtom Succ = modify (first succ)
interpAtom Pred = do
  (p,_) <- get
  when (p==0) (error "Pred for ptr==0")
  modify (first pred)
interpAtom com | com == Inc || com == Dec = do
  val <- getVal
  let op = case com of Inc -> (+); Dec -> (-)
      val' = chr $ validate (ord val `op` 1)
  putVal val'
interpAtom PutChar = do
  val <- getVal
  liftIO $ putChar val
interpAtom GetChar = do
  val <- liftIO getChar
  putVal val
interpAtom (While ls) = do
  val <- getVal
  if (ord val == 0)
    then return ()
    else do
      interp ls
      interpAtom (While ls)

interp :: [Command] ->  BFM ()
interp = mapM_ interpAtom

interp' :: [Command] ->  BFM ()
interp' = mapM_ interpAtom'

runBFM :: BFM () -> IO ()
runBFM bfm = evalStateT bfm (0,Map.empty)

commandToProg :: [Command] -> Prog
commandToProg = id