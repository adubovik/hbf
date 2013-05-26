{-# LANGUAGE
   NoMonomorphismRestriction #-}

module BrainFuckDSL where

import Prelude hiding (getChar)
import Control.Arrow
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.Char
import qualified Data.Set as Set

import Types

type MemoryState = Set.Set Int

type VarM m a = StateT (Var, Int)
                (WriterT [Command] m) a

data Var = Var Int

curr :: Monad m => VarM m Var
curr = do
  (v,_) <- get
  return v

newVar :: Monad m => VarM m Var
newVar = do
  (_,n) <- get
  modify (id *** succ)
  return $ Var n

switch :: Monad m => Var -> VarM m ()
switch v@(Var n1) = do
  (Var n2,_) <- get
  let dir True  = Succ
      dir False = Pred
  tell $ replicate (abs $ n1-n2) (dir (n1 > n2))
  modify (const v *** id)

infix 5 +:
(+:) :: Monad m => Var -> Int -> VarM m ()
v +: n = switcher v $ do
  let sign True = Inc
      sign False = Dec
  tell $ replicate (abs n) (sign (n>0))

switcher :: Monad m => Var -> VarM m a -> VarM m ()
switcher v act = do
  (old_v,_) <- get
  switch v
  act
  switch old_v

putchar :: Monad m => Var -> VarM m ()
putchar v = switcher v $ tell [PutChar]
getchar v = switcher v $ tell [GetChar]

while :: Monad m => Var -> VarM m a -> VarM m ()
while v act = switcher v $
  pass $ do
    act
    return ((), \w -> [While w])

delVar :: Monad m => Var -> VarM m a
delVar = undefined

localVar :: Monad m => (Var -> VarM m a) -> VarM m a
localVar f = do
  tmp <- newVar
  x <- f tmp
  delVar tmp
  return x

----------- Lib --------

zero v = while v (v +: (-1))

add src dst = while src $ do
  src +: (-1)
  dst +:   1

mov from to = do
  zero to
  add from to

copy from to = do
  zero to
  tmp <- newVar
  while from $ do
    from +: (- 1)
    to   +:    1
    tmp  +:    1
  mov tmp from

ifthen v t f = do
  tmp <- newVar
  tmp +: 1
  v' <- newVar
  copy v v'
  while v' $ do
    t
    zero tmp
    zero v'
  while tmp $ do
    f
    zero tmp

----------- Clients ----------

printChar c = do
  v <- newVar
  v +: (ord c)
  putchar v

(-|) v n = do
  n' <- newVar
  copy n n'
  while n' $ do
    n' +: (-1)
    ifthen v (v +: (-1)) (return ())

-- v := v `div` 2
div2 v = do
  tmp <- newVar
  two <- newVar
  two +: 2
  one <- newVar
  one +: 1
  v -| one
  while v $ do
    v   -| two
    tmp +: 1
  mov tmp v

-- v := v `mod` 2
mod2 v = do
  tmp <- newVar
  copy v tmp
  div2 tmp
  v -| tmp
  v -| tmp

binaryOut :: Monad m => Int -> Var -> VarM m ()
binaryOut 0 v = return ()
binaryOut n v = do
  r <- newVar
  q <- newVar
  copy v r
  copy v q
  div2 q
  mod2 r
  binaryOut (n-1) q
  ifthen r (printChar '*') (printChar '-')

encodeChar :: Monad m => VarM m ()
encodeChar = do
  v <- newVar
  getchar v
  v +: (- (ord 'a'))
  binaryOut 5 v
  printChar '\n'

------------- Frontend ---------

runVarM :: Monad m => VarM m () -> m [Command]
runVarM act = do
  (_,w) <- runWriterT $ evalStateT act (Var 0,0)
  return w

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

