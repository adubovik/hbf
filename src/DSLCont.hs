{-# language
   TypeSynonymInstances
 , FlexibleInstances
 , TypeFamilies
 #-}

module DSLCont
  ( u
  , (<!>)
  )
where

import Control.Monad.Cont

import DSL
import DSL.Lib

type C = ContT ()

u :: VarD r -> C r (VarD r)
u = return

infixl 1 <!>

(<!>) :: DSL r => (a -> r ()) -> C r a -> r ()
(<!>) = flip runContT

binOpCont :: DSL r =>
        (VarD r -> VarD r -> r ())
     -> C r (VarD r)
     -> C r (VarD r)
     -> C r (VarD r)
binOpCont op x y = do
    t1 <- x
    t2 <- y
    ContT $ \k ->
      localVar $ \t0 -> do
        t0  =: t1
        t0 `op` t2
        k t0

instance (DSL r, v ~ VarD r) => Num (C r v) where
  (+) = binOpCont (+=:)
  (-) = binOpCont (-=:)
  (*) = binOpCont (*=:)
  
  fromInteger i = ContT $ \k ->
    localVar $ \t -> do
      t += (fromIntegral i)
      k t
  
  abs    = undefined
  signum = undefined

-- FIXME: Type classes hell!
instance (DSL r, v ~ VarD r) => Enum (C r v) where
  toEnum   = undefined
  fromEnum = undefined
instance (DSL r, v ~ VarD r) => Eq   (C r v) where
instance (DSL r, v ~ VarD r) => Ord  (C r v) where
instance (DSL r, v ~ VarD r) => Real (C r v) where
  toRational = undefined

instance (DSL r, v ~ VarD r) => Integral (C r v) where
  quotRem a b = (binOpCont (/=:) a b, binOpCont (%=:) a b)
  divMod      = quotRem

  toInteger = undefined
