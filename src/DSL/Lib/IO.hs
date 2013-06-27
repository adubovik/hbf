module DSL.Lib.IO where

import Data.Char

import DSL
import DSL.Lib.Core
import DSL.Lib.Array
import DSL.Lib.Arithmetics

printChar :: DSL r => Char -> r ()
printChar c = do
  localVar $ \v -> do
    v += ord c
    putchar v

-- | reads integer to variable `v` where space is used as delimiter.
-- There is no end-of-file marker in BF.
readInt :: DSL r => VarD r -> r ()
readInt v = do
  zero v
  localVar $ \space -> do
    space += ord ' '
    localVar $ \flag -> do
      flag += 1
      while flag $ do
        localVar $ \char -> do
          getchar char
          localVar $ \isspace -> do
            isspace =: char
            isspace ===: space
            ifthen isspace (zero flag) $ do
              char -= ord '0'
              localVar $ \ten -> do
                ten += 10
                v *=: ten
                v +=: char

printIntN :: DSL r => Int -> VarD r -> r ()
printIntN n v =
  localVar $ \nil -> do
    nil += ord '0'
    ifthen v (printIntPosN n v) (putchar nil)

-- | for (n>0) only
printIntPosN :: DSL r => Int -> VarD r -> r ()
printIntPosN n v = localArr (n+1) $ \arr -> do
  localVar $ \idx -> do
    while v $ do
      localVar $ \r -> do
        r =: v
        remainder 10 r
        quotient  10 v

        setArrayCell idx r arr
      idx += 1

    while idx $ do
      idx -= 1
      localVar $ \r -> do
        getArrayCell r idx arr
        r += ord '0'
        putchar r

-- | default version only for numbers less than 10^4
printInt :: DSL r => VarD r -> r ()
printInt = printIntN 4