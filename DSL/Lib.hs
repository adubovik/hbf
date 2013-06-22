{-# language
   FlexibleContexts #-}

module DSL.Lib where

import Data.Char
import Control.Monad

import DSL
import Types
import Parser

----------- Very base ----------

infix 5 +:
(+:) :: DSL m => VarD m -> Int -> m ()
v +: n = do
  let sign True  = inc v
      sign False = dec v
  replicateM_ (abs n) (sign (n>0))

zero :: DSL r => VarD r -> r ()
zero v = while v (v +: (-1))

add :: DSL r => VarD r -> VarD r -> r ()
add src dst = while src $ do
  src +: (-1)
  dst +:   1

mov :: DSL m => VarD m -> VarD m -> m ()
mov from to = do
  zero to
  add from to

copy :: DSL m => VarD m -> VarD m -> m ()
copy from to = do
  zero to
  localVar' "copy_tmp" $ \tmp -> do
    while from $ do
      from +: (- 1)
      to   +:    1
      tmp  +:    1
    mov tmp from

ifthen :: DSL r => VarD r -> r () -> r () -> r ()
ifthen v t f = do
  localVar' "ifthen_tmp" $ \tmp -> do
    tmp +: 1
    localVar' "ifthen_v" $ \v' -> do
      copy v v'
      while v' $ do
        t
        zero tmp
        zero v'
      while tmp $ do
        f
        zero tmp

----------- Advanced ----------

printChar :: DSL r => Char -> r ()
printChar c = do
  localVar' "prtChar_v" $ \v -> do
    v +: (ord c)
    putchar v

(-|) :: DSL r => VarD r -> VarD r -> r ()
(-|) v n = do
  localVar' "abssub_n'" $ \n' -> do
    copy n n'
    while n' $ do
      n' +: (-1)
      ifthen v (v +: (-1)) (return ())

-- v := v `div` 2
div2 :: DSL r => VarD r -> r ()
div2 v = do
  localVar' "div2_tmp" $ \tmp -> do
    localVar' "div2_two" $ \two -> do
      two +: 2
      localVar' "div2_one" $ \one -> do
        one +: 1
        v -| one
        while v $ do
          v   -| two
          tmp +: 1
        mov tmp v

-- before: >n d
-- after : >0 d-n%d n%d n/d
-- origin: [->-[>+>>]>[+[-<+>]>+>>]<<<<<]
-- [n0- n1- n2+ { if n1 () else ([n2- n1+] n3+)} <<<<<]
divmod :: DSL r =>
          VarD r -> VarD r -> VarD r -> VarD r -> r ()
divmod n d r q =
  localVar' "div_n'" $ \n' -> do
    localVar' "div_d'" $ \d' -> do
          copy n n'
          copy d d'
    
          zero r
          zero q
          
          while n' $ do
            n' +: (-1)
            d' +: (-1)
            r  +: 1
            ifthen d' (return ()) $ do
              while r $ do
                r  +: (-1)
                d' +: 1
              q +: 1

-- v := v `mod` 2
mod2 :: DSL r => VarD r -> r ()
mod2 v = do
  localVar' "mod2_tmp" $ \tmp -> do
    copy v tmp
    div2 tmp
    v -| tmp
    v -| tmp

repeatCode :: DSL r => Int -> r () -> r ()
repeatCode n act = do
  localVar' "repeatCnt" $ \cnt -> do
    cnt +: n
    while cnt $ do
      cnt +: (-1)
      act

setVar :: DSL m => VarD m -> Char -> m ()
setVar v c = do
  zero v
  v +: (ord c)

binaryOutArr :: DSL r => Int -> VarD r -> r ()
binaryOutArr n v = localArr' "binOut_arr" n $ \arr -> do
  localVar' "binOut_idx" $ \idx -> do
    repeatCode n $ do
      localVar' "binOut_r1" $ \r -> do        
        localVar' "binOut_2" $ \two -> do
          two +: 2
          divmod v two r v
        setArrayCell idx r arr
        idx +: 1
    repeatCode n $ do
      localVar' "binOut_r2" $ \r -> do
        idx +: (-1)
        getArrayCell r idx arr

        -- output
        localVar' "binOut_char" $ \c -> do
          c +: (ord '*')
          ifthen r (return ()) (c +: (ord '-' - ord '*'))
          putchar c
        
binaryOutHavy :: DSL r => Int -> VarD r -> r ()
binaryOutHavy 0 _ = return ()
binaryOutHavy n v = do
  localVar' "binOut_r" $ \r -> do
    copy v r
    mod2 r
    div2 v
    binaryOutHavy (n-1) v

    ifthen r (printChar '*') (printChar '-')

binaryOut :: DSL r => Int -> VarD r -> r ()
binaryOut = binaryOutArr

encodeChar :: DSL r => Int -> r ()
encodeChar n = do
  localVar' "encChar_v" $ \v -> do
    getchar v
    v +: (- (ord 'a'))
    binaryOut n v
    printChar '\n'

encodeString :: DSL r => r ()
encodeString = repeatCode 26 (encodeChar 5)

----- Array machinery -----

--------------
-- Small lib

-- Look at
-- http://esolangs.org/wiki/brainfuck_algorithms#x.28y.29_.3D_z_.281-d_array.29_.282_cells.2Farray_element.29
-- for reference
-- 
-- arr(idx) = src
-- FIXME: handle array out of bounds error
-- idx -> src -> arr
setArrayCell :: DSL r => VarD r -> VarD r -> ArrayD r -> r ()
setArrayCell idx src arr = do
  zero $ arrT0 arr
  zero $ arrT1 arr
  copy idx $ arrT1 arr
  copy src $ arrT0 arr

  switch $ arrInit arr

  unsafeBF ">>"
  unsafeBF "[[>>]+[<<]>>-]"
  unsafeBF "+[>>]<[-]<[<<]"
  unsafeBF ">[>[>>]<+<[<<]>-]"
  unsafeBF ">[>>]<<[-<<]"

-- http://esolangs.org/wiki/brainfuck_algorithms#x_.3D_y.28z.29_.281-d_array.29_.282_cells.2Farray_element.29
-- dest = arr(idx)          
-- dst -> idx -> arr
getArrayCell :: DSL r => VarD r -> VarD r -> ArrayD r -> r ()
getArrayCell dst idx arr = do
  zero dst
  zero $ arrT0 arr
  zero $ arrT1 arr
  copy idx $ arrT1 arr

  switch $ arrInit arr

  unsafeBF ">>"
  unsafeBF "[[>>]+[<<]>>-]"
  unsafeBF "+[>>]<"

  whileU $ do
    unsafeBF "<[<<]>+<"
    switch dst
    incU
    switch $ arrInit arr
    unsafeBF ">>[>>]<-"

  unsafeBF "<[<<]>"
  unsafeBF "[>[>>]<+<[<<]>-]"
  unsafeBF ">[>>]<<[-<<]"

zeroUnsafe :: DSL r => r ()
zeroUnsafe = whileU decU

zeroArray :: DSL r => ArrayD r -> r ()
zeroArray arr = do
  switch (arrInit arr)
  replicateM_ (arrLength arr) $ do
    zeroUnsafe
    succU
  replicateM_ (arrLength arr) $ do
    predU

-----

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

inc, dec :: DSL r => VarD r -> r ()
dec v = switch v >> decU
inc v = switch v >> incU

putchar, getchar :: DSL r => VarD r -> r ()
putchar v = switch v >> putcharU
getchar v = switch v >> getcharU

while :: DSL r => VarD r -> r () -> r ()
while v act = do
  switch v
  whileU (act >> switch v)