{-# language
   TypeSynonymInstances
 , FlexibleInstances
 , TypeFamilies
 #-}

module DSL.Expression
  ( u
  , (<!>)
  , execDSLExpr

  , printI
  , printC
  , readI
  , newVar
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

execDSLExpr :: DSL r => C r a -> r ()
execDSLExpr x = (const $ return ()) <!> x

binOpCont :: DSL r =>
        (VarD r -> VarD r -> r ())
     -> C r (VarD r)
     -> C r (VarD r)
     -> C r (VarD r)
binOpCont op xc yc = do
    x <- xc
    y <- yc
    ContT $ \k ->
      localVar $ \t -> do
        t  =: x
        t `op` y
        k t

-- | Pure actions

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

-- | IO
  
readI :: DSL r => C r (VarD r)
readI = ContT $ \k ->
  localVar $ \t -> do
    readInt t
    k t

printI :: DSL r => C r (VarD r) -> C r ()
printI xc = do
  x <- xc
  ContT $ \k -> do
    printInt x
    k ()

printC :: DSL r => Char -> C r ()
printC c = ContT $ \k -> do
  printChar c
  k ()

-- | Rest

newVar :: DSL r => C r (VarD r)
newVar = ContT $ \k ->
  localVar $ \t0 -> do
    k t0
