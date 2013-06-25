module DSL.Lib where

import Data.Char
import Control.Monad

import DSL
import Types
import Parser

-- | Arithmetics

infix 5 +=
infix 5 -=

(+=),(-=) :: DSL m => VarD m -> Int -> m ()
v += n = do
  let sign True  = inc v
      sign False = dec v
  replicateM_ (abs n) (sign (n>0))

v -= n = v += (-n)

add :: DSL r => VarD r -> VarD r -> r ()
add src dst = while src $ do
  src -= 1
  dst += 1

zero :: DSL r => VarD r -> r ()
zero v = while v (v -= 1)

abssub :: DSL r => VarD r -> VarD r -> r ()
abssub v n = do
  localVar $ \n' -> do
    n' =: n
    while n' $ do
      n' -= 1
      ifthen v (v -= 1) (return ())

(-|) :: DSL r => VarD r -> VarD r -> r ()
(-|) = abssub

-- v =: v `div` n
quotient :: DSL r => Int -> VarD r -> r ()
quotient n v = do
  localVar $ \tmp -> do
    localVar $ \num -> do
      num += n
      localVar $ \one -> do
        one += 1
        v -| one
        while v $ do
          v -| num
          tmp += 1
        mov tmp v

-- before: >n d
-- after : >0 d-n%d n%d n/d
-- origin: [->-[>+>>]>[+[-<+>]>+>>]<<<<<]
-- [n0- n1- n2+ { if n1 () else ([n2- n1+] n3+)} <<<<<]
divmod :: DSL r =>
          VarD r -> VarD r -> VarD r -> VarD r -> r ()
divmod n d r q =
  localVar $ \n' -> do
    localVar $ \d' -> do
      n' =: n
      d' =: d
        
      zero r
      zero q

      while n' $ do
        n' -= 1
        d' -= 1
        r  += 1
        ifthen d' (return ()) $ do
          while r $ do
            r  -= 1
            d' += 1
          q += 1

-- v := v `mod` n
remainder :: DSL r => Int -> VarD r -> r ()
remainder n v = do
  localVar $ \tmp -> do
    tmp =: v
    quotient n tmp
    repeatCode n $
      v -| tmp

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
