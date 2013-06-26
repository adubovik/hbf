module DSL.Lib.Arithmetics where

import DSL
import DSL.Lib.Core

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
