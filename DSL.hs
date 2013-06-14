{-# LANGUAGE
   TypeFamilies
 , FlexibleContexts #-}

module DSL where

class Variable r where
  type Array r :: *
  arrT0, arrT1, arrInit :: Array r -> r
  arrLength :: Array r -> Int
  arrMake :: r -> Int -> Array r

type ArrayD r = Array (VarD r)

class (Monad r, Variable (VarD r)) => DSL r where
  type VarD r :: *
  
  localVar ::        (  VarD r -> r ()) -> r ()
  localArr :: Int -> (ArrayD r -> r ()) -> r ()

  -- annotated versions
  localVar' :: String ->        (  VarD r -> r ()) -> r ()
  localVar' = const localVar
  
  localArr' :: String -> Int -> (ArrayD r -> r ()) -> r ()
  localArr' = const localArr

  while :: VarD r -> r () -> r ()
  switch :: VarD r -> r ()

  --- Unsafes
  incU, decU :: r ()
  predU, succU :: r ()
  putcharU, getcharU :: r ()
  whileU :: r a -> r ()