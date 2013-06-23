{-# LANGUAGE
   TypeFamilies
 , FlexibleContexts #-}

module DSL
  ( ArrVar(..)
  , ArrayD
  , Arr(..)
  , DSL(..)
  )
where
  
data Arr v = Arr v Int deriving Show

class ArrVar r where
  arrT0, arrT1, arrInit :: Arr r -> r
  arrLength :: Arr r -> Int
  arrMake :: r -> Int -> Arr r

type ArrayD r = Arr (VarD r)

class (Monad r, ArrVar (VarD r)) => DSL r where
  type VarD r :: *
  
  localVar ::        (  VarD r -> r ()) -> r ()
  localArr :: Int -> (ArrayD r -> r ()) -> r ()

  -- annotated versions
  localVar' :: String ->        (  VarD r -> r ()) -> r ()
  localVar' = const localVar
  
  localArr' :: String -> Int -> (ArrayD r -> r ()) -> r ()
  localArr' = const localArr

  switch :: VarD r -> r ()

  --- Essential BF commands
  incU, decU :: r ()
  predU, succU :: r ()
  putcharU, getcharU :: r ()
  whileU :: r () -> r ()