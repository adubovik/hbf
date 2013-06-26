module DSL.Lib.Core where

import Data.Char
import Control.Monad

import DSL
import Types
import Parser

-- | Base arithmetics

infix 5 +=
infix 5 -=

(+=),(-=) :: DSL m => VarD m -> Int -> m ()
v += n = do
  let sign True  = inc v
      sign False = dec v
  replicateM_ (abs n) (sign (n>0))

v -= n = v += (-n)

zero :: DSL r => VarD r -> r ()
zero v = while v (v -= 1)

add :: DSL r => VarD r -> VarD r -> r ()
add src dst = while src $ do
  src -= 1
  dst += 1

-- | Moving bites around

mov :: DSL m => VarD m -> VarD m -> m ()
mov from to = do
  zero to
  add from to

copy :: DSL m => VarD m -> VarD m -> m ()
copy from to = do
  zero to
  localVar $ \tmp -> do
    while from $ do
      from -= 1
      to   += 1
      tmp  += 1
    mov tmp from

infix 5 =:
(=:) :: DSL m => VarD m -> VarD m -> m ()
(=:) = flip copy

-- | Unsafe actions

unsafeBF :: DSL r => String -> r ()
unsafeBF prog = 
  case comm of
    Left e      -> error $ "unsafeBF: " ++ show e
    Right prog' -> commandToDSL prog'
  where
    comm = parseBF prog

commandToDSL :: DSL r => [Command] -> r ()
commandToDSL = mapM_ f
  where
    f Pred = predU
    f Succ = succU
    f Inc  = incU
    f Dec  = decU
    f GetChar = getcharU
    f PutChar = putcharU
    f (While l) = whileU (commandToDSL l)

-- | DSL enhancements

inc, dec :: DSL r => VarD r -> r ()
dec v = switch v >> decU
inc v = switch v >> incU

putchar, getchar :: DSL r => VarD r -> r ()
putchar v = switch v >> putcharU
getchar v = switch v >> getcharU

-- | Control flow

ifthen :: DSL r => VarD r -> r () -> r () -> r ()
ifthen v t f = do
  localVar $ \tmp -> do
    tmp += 1
    localVar $ \v' -> do
      v' =: v
      while v' $ do
        t
        zero tmp
        zero v'
      while tmp $ do
        f
        zero tmp

repeatCode :: DSL r => Int -> r () -> r ()
repeatCode n act = do
  localVar $ \cnt -> do
    cnt += n
    while cnt $ do
      cnt -= 1
      act

while :: DSL r => VarD r -> r () -> r ()
while v act = do
  switch v
  whileU (act >> switch v)

-- | IO
printChar :: DSL r => Char -> r ()
printChar c = do
  localVar $ \v -> do
    v += ord c
    putchar v
