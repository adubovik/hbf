module DSL.Lib.EncodeString
  ( encodeChar
  , encodeString
  )
where

import Data.Char

import DSL
import DSL.Lib.Core
import DSL.Lib.Array
import DSL.Lib.Arithmetics
import DSL.Lib.IO

-- | Derivative
binaryOut :: DSL r => Int -> VarD r -> r ()
binaryOut n v = localArr n $ \arr -> do
  localVar $ \idx -> do
    forI n $ do
      localVar $ \r -> do        
        localVar $ \two -> do
          two += 2
          divmod v two r v
        setArrayCell idx r arr
        idx += 1
    forI n $ do
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
encodeString = forI 26 (encodeChar 5)
