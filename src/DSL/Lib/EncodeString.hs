module DSL.Lib.EncodeString
  ( encodeChar
  , encodeString
  )
where

import Data.Char

import DSL
import DSL.Lib
import DSL.Lib.Array

-- | Derivative
binaryOut :: DSL r => Int -> VarD r -> r ()
binaryOut n v = localArr n $ \arr -> do
  localVar $ \idx -> do
    repeatCode n $ do
      localVar $ \r -> do        
        localVar $ \two -> do
          two += 2
          divmod v two r v
        setArrayCell idx r arr
        idx += 1
    repeatCode n $ do
      localVar $ \r -> do
        idx -= 1
        getArrayCell r idx arr

        -- output
        localVar $ \c -> do
          c += ord '*'
          ifthen r (return ()) (c += (ord '-' - ord '*'))
          putchar c
        
encodeChar :: DSL r => Int -> r ()
encodeChar n = do
  localVar $ \v -> do
    getchar v
    v -= ord 'a'
    binaryOut n v
    printChar '\n'

encodeString :: DSL r => r ()
encodeString = repeatCode 26 (encodeChar 5)
