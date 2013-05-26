{-# LANGUAGE
   TypeFamilies #-}

module DSL where

class DSL r where
  type   VarD r :: *
  type ArrayD r :: *
  
  localVar ::        (  VarD r -> r a) -> r a
  localArr :: Int -> (ArrayD r -> r a) -> r a

  -- annotated versions
  localVar' :: String ->        (  VarD r -> r a) -> r a
  localVar' = const localVar
  
  localArr' :: String -> Int -> (ArrayD r -> r a) -> r a
  localArr' = const localArr

  inc, dec :: VarD r -> r ()
  putchar, getchar :: VarD r -> r ()

  -- idx -> src -> arr
  setArrayCell :: VarD r -> VarD r -> ArrayD r -> r ()
  -- dst -> idx -> arr
  getArrayCell :: VarD r -> VarD r -> ArrayD r -> r ()

  while :: VarD r -> r a -> r ()
