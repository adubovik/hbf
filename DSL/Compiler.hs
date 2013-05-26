{-# LANGUAGE
   NoMonomorphismRestriction
 , TypeFamilies
 , TypeSynonymInstances
 , FlexibleInstances #-}

module DSL.Compiler where

import Prelude hiding (succ, pred)
import Control.Arrow
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.Char
import qualified Data.Set as Set

import Types
import Utils
import Parser

import DSL
import DSL.Lib

-------------------------------------------------
-- Memory infrastructure

type MemoryState = Set.Set Int

type VarM m = StateT (Var, MemoryState)
              (WriterT [Command] m)

data Var = Var Int

opVar :: (Int -> Int) -> (Var -> Var)
opVar f (Var i) = Var (f i)

data Array = Array { headPtr   :: Var
                   , t0, t1    :: Var
                   , arrLength :: Int }

arrSize :: Int -> Int
arrSize n = 2*n + 3

-------------------------------------------------
-- Memory managment

mkVariable :: MemoryState -> (Var, MemoryState)
mkVariable mem = (var, mem')
  where
    cells    = Set.toList mem
    freeCell = head $ filter (not . (`elem` cells)) [0..]
    var      = Var freeCell
    mem'     = Set.insert freeCell mem

delVariable :: Var -> MemoryState -> MemoryState
delVariable (Var cell) mem =
  Set.delete cell mem

mkArray :: Int -> MemoryState -> (Array, MemoryState)
mkArray n mem = (arr, mem')
  where
    m = arrSize n
    arrCands = map (\x -> [x+1..x+m]) ((-1) : Set.toList mem)
    arrCands'= filter (not . any (`Set.member` mem)) arrCands
    arrCells = head arrCands'
    initCell = head arrCells
    arr      = Array (Var $ initCell+0)
                     (Var $ initCell+1)
                     (Var $ initCell+2) n
    mem'     = mem <> (Set.fromList arrCells)

delArray :: Array -> MemoryState -> MemoryState
delArray (Array (Var initCell) _ _ n) mem =
  let str = initCell
      end = str + arrSize n - 1
  in  mem `Set.difference` Set.fromList [str,end]

-------------------------------------------------
-- Base DSL

instance Monad m => DSL (VarM m) where
  type VarD   (VarM m) = Var
  type ArrayD (VarM m) = Array
  
  localArr n f = do
    tmp <- mkArr n
    x <- f tmp
    delArr tmp
    return x
  
  localVar f = do
    tmp <- newVar
    x <- f tmp
    delVar tmp
    return x

  dec v = switch v >> tell [Dec]
  inc v = switch v >> tell [Inc]

  putchar v = switch v >> tell [PutChar]
  getchar v = switch v >> tell [GetChar]

  -- Look at
  -- http://esolangs.org/wiki/brainfuck_algorithms#x.28y.29_.3D_z_.281-d_array.29_.282_cells.2Farray_element.29
  -- for reference
  -- 
  -- arr(idx) = src
  -- FIXME: handle out of array bounds error
  setArrayCell idx src (Array init t0 t1 _) = do
    zero t0
    zero t1
    copy idx t1
    copy src t0

    switcher init $ do
      unsafeBF ">>"
      unsafeBF "[[>>]+[<<]>>-]"
      unsafeBF "+[>>]<[-]<[<<]"
      unsafeBF ">[>[>>]<+<[<<]>-]"
      unsafeBF ">[>>]<<[-<<]"

  -- http://esolangs.org/wiki/brainfuck_algorithms#x_.3D_y.28z.29_.281-d_array.29_.282_cells.2Farray_element.29
  -- dest = arr(idx)          
  getArrayCell dst idx (Array init t0 t1 _) = do
    zero dst
    zero t0
    zero t1
    copy idx t1

    switcher init $ do
      unsafeBF ">>"
      unsafeBF "[[>>]+[<<]>>-]"
      unsafeBF "+[>>]<"

      whileUnsafe $ do
        unsafeBF "<[<<]>+<"
        switcher dst $ incUnsafe
        unsafeBF ">>[>>]<-"

      unsafeBF "<[<<]>"
      unsafeBF "[>[>>]<+<[<<]>-]"
      unsafeBF ">[>>]<<[-<<]"

  while v act = switcher v $
    pass $ do
      act
      switch v
      return ((), return . While)

-------------------------------------------------
-- Array machinary

mkArr :: Monad m => Int -> VarM m Array
mkArr n = do
  (_,mem) <- get
  let (arr,mem') = mkArray n mem
  modify (second $ const mem')
  return arr

zeroArray (Array initVar _ _ n) = switcher initVar $
  replicateM_ (arrSize n) $ do
    zeroUnsafe
    succ

delArr :: Monad m => Array -> VarM m ()
delArr arr = do
  zeroArray arr
  modify (second $ delArray arr)

unsafeBF :: Monad m => String -> VarM m ()
unsafeBF prog = 
  case comm of
    Left e      -> error $ "Error: " ++ show e
    Right prog' -> commandToDSL prog'
  where
    comm = parseBF prog
    commandToDSL = mapM_ f
      where
        f Pred = predUnsafe
        f Succ = succUnsafe
        f Inc  = incUnsafe
        f Dec  = decUnsafe
        f GetChar = getcharUnsafe
        f PutChar = putcharUnsafe
        f (While l) = whileUnsafe (commandToDSL l)

-------------------------------------------------
-- Rest

curr :: Monad m => VarM m Var
curr = do
  (v,_) <- get
  return v

newVar :: Monad m => VarM m Var
newVar = do
  (_,mem) <- get
  let (var, mem') = mkVariable mem
  modify (second $ const mem')
  return var

delVar :: Monad m => Var -> VarM m ()
delVar v = do
  zero v
  modify (second $ delVariable v)

switch :: Monad m => Var -> VarM m ()
switch (Var n1) = do
  (Var n2,_) <- get
  let dir True  = succ
      dir False = pred
  replicateM_ (abs $ n1-n2) (dir (n1 > n2))

switcher :: Monad m => Var -> VarM m a -> VarM m ()
switcher v act = do
  (old_v,_) <- get
  switch v
  act
  switch old_v

putcharUnsafe, getcharUnsafe :: Monad m => VarM m ()
putcharUnsafe = tell [PutChar]
getcharUnsafe = tell [GetChar]

decUnsafe, incUnsafe :: Monad m => VarM m ()
decUnsafe = tell [Dec]
incUnsafe = tell [Inc]

succ, pred :: Monad m => VarM m ()
succ = do
  tell [Succ]
  modify (first $ opVar (+1))
pred = do
  tell [Pred]
  -- FIX: handle pointer out of bound (<0)
  modify (first $ opVar (+ negate 1))

succUnsafe, predUnsafe :: Monad m => VarM m ()
succUnsafe = tell [Succ]
predUnsafe = tell [Pred]

whileUnsafe :: Monad m => VarM m a -> VarM m ()
whileUnsafe act =
  pass $ do
    act
    return ((), return . While)

zeroUnsafe :: Monad m => VarM m ()
zeroUnsafe = whileUnsafe decUnsafe

------------- Frontend ---------

runVarM :: Monad m => VarM m () -> m [Command]
runVarM act = do
  (_,w) <-  runWriterT $ evalStateT act $
           (Var 0, Set.empty)
  return $ optBF w

-- shrinks sequences of same command
prettySrcDense :: [Command] -> String
prettySrcDense [] = []
prettySrcDense (While ls : r) =
  "[" ++ prettySrcDense ls ++ "]" ++ prettySrcDense r
prettySrcDense r
  | ln > 1 = show (length a) ++ [prettyAtom h] ++ prettySrcDense r'
  | otherwise = prettyAtom h : prettySrcDense r'
  where
    h = head r
    ln = length a
    (a,r') = span (== h) r
    
prettyAtom Succ    = '>'
prettyAtom Pred    = '<'
prettyAtom Inc     = '+'
prettyAtom Dec     = '-'
prettyAtom PutChar = '.'
prettyAtom GetChar = ','

prettySrc :: [Command] -> String
prettySrc = concatMap pretty
  where
    pretty Succ    = ">"
    pretty Pred    = "<"
    pretty Inc     = "+"
    pretty Dec     = "-"
    pretty PutChar = "."
    pretty GetChar = ","
    pretty (While ls) = "[" ++ prettySrc ls ++ "]"

