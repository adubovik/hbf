{-# LANGUAGE
   NoMonomorphismRestriction
 , TypeFamilies
 , TypeSynonymInstances
 , FlexibleInstances
 , ViewPatterns
 , ScopedTypeVariables #-}

module DSL.Compiler where

import Prelude hiding (succ, pred)
import Control.Arrow
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.Char
import qualified Data.Set as Set

import Types

import DSL
import DSL.Lib

-------------------------------------------------
-- Memory infrastructure

type MemoryState = Set.Set Int

type VarM m = StateT (Var, MemoryState)
              (WriterT [Command] m)

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

mkArray :: Int -> MemoryState -> (Arr, MemoryState)
mkArray n mem = (arr, mem')
  where
    m = arrSize n
    arrCands = map (\x -> [x+1..x+m]) ((-1) : Set.toList mem)
    arrCands'= filter (not . any (`Set.member` mem)) arrCands
    arrCells = head arrCands'
    initCell = head arrCells
    arr      = Arr (Var initCell) n
    mem'     = mem <> (Set.fromList arrCells)

delArray :: Arr -> MemoryState -> MemoryState
delArray (Arr (Var initCell) n) mem =
  let str = initCell
      end = str + arrSize n - 1
  in  mem `Set.difference` Set.fromList [str,end]

-------------------------------------------------
-- Base DSL

instance Monad m => DSL (VarM m) where
  type VarD   (VarM m) = Var
  
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

  switch (ptrOfVar -> n1) = do
    (ptrOfVar -> n2,_) <- get
    let dir True  = succ
        dir False = pred
    replicateM_ (abs $ n1-n2) (dir (n1 > n2))

  dec v = switch v >> tell [Dec]
  inc v = switch v >> tell [Inc]

  putchar v = switch v >> tell [PutChar]
  getchar v = switch v >> tell [GetChar]

  while v act = do
    switch v
    pass $ do
      act
      switch v
      return ((), return . While)

  decU = tell [Dec]
  incU = tell [Inc]

  putcharU = tell [PutChar]
  getcharU = tell [GetChar]

  succU = tell [Succ]
  predU = tell [Pred]

  whileU act =
    pass $ do
      act
      return ((), return . While)

-------------------------------------------------
-- Array machinary

mkArr :: Monad m => Int -> VarM m Arr
mkArr n = do
  (_,mem) <- get
  let (arr,mem') = mkArray n mem
  modify (second $ const mem')
  return arr

zeroArray :: (Monad m) => Arr -> VarM m ()
zeroArray (Arr ptr n) = do
  switch ptr
  replicateM_ n $ do
    zeroUnsafe
    succ

delArr :: Monad m => Arr -> VarM m ()
delArr arr = do
  zeroArray arr
  modify (second $ delArray arr)

-------------------------------------------------
-- Rest

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

succ, pred :: Monad m => VarM m ()
succ = do
  tell [Succ]
  modify (first $ opVar (+1))
pred = do
  tell [Pred]
  -- FIX: handle pointer out of bound (<0)
  modify (first $ opVar (+ negate 1))

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

