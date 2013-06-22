{-# language
   TypeFamilies #-}

module Types
  ( Var(..)
  , Command(..)
  , normVar
  , mapVar
  , opVar

  , optBF
  )
where

import DSL

data Var = Var Int
         | ArrT0 Int
         | ArrT1 Int
         | ArrInit Int
         deriving (Show, Eq, Ord)

normVar :: Var -> Var
normVar (Var     i) = Var $ i
normVar (ArrInit i) = Var $ i+0
normVar (ArrT0   i) = Var $ i+1
normVar (ArrT1   i) = Var $ i+2

mapVar :: (Int -> Int) -> Var -> Var
mapVar f (Var i) = Var $ f i
mapVar f (ArrInit i) = ArrInit $ f i
mapVar f (ArrT0 i) = ArrT0 $ f i
mapVar f (ArrT1 i) = ArrT1 $ f i

opVar :: (Int -> Int) -> (Var -> Var)
opVar f (Var i) = Var (f i)
opVar _ v = error $ "opVar f (" ++ show v ++ ")"

instance ArrVar Var where
  arrInit (Arr (Var i) _) = ArrInit i
  arrInit v = error $ "arrInit (" ++ show v ++ ")"
  
  arrT0 (Arr (Var i) _) = ArrT0 i
  arrT0 v = error $ "arrT0 (" ++ show v ++ ")"
  
  arrT1     (Arr (Var i) _) = ArrT1 i
  arrT1 v = error $ "arrT1 (" ++ show v ++ ")"
  
  arrLength (Arr _ n)       = 2*n+3
  arrMake                   = Arr

data Command = Succ | Pred |
               Inc | Dec |
               PutChar | GetChar |
               While [Command]
               deriving (Show, Eq)

optBF :: [Command] -> [Command]
optBF cmds = optReduce (Succ,Pred) .
             optReduce (Inc,Dec) .
             subOpt $ cmds
  where
    subOpt [] = []
    subOpt (While ls : xs) = While (optBF ls) : optBF xs
    subOpt (PutChar:xs) = PutChar : optBF xs
    subOpt (GetChar:xs) = GetChar : optBF xs
    subOpt x = x
    
    optReduce _ [] = []
    optReduce (a,b) xs
      | isOper $ head xs = reducePtrOp cs ++ optBF rest
      | otherwise = xs
      where
        (cs,rest) = span isOper xs
        isOper x = x == a || x == b
        reducePtrOp ls = replicate (abs $ as - bs) comm
          where
            comm = if (as > bs) then a else b
            as = length $ filter (==a) ls
            bs = length $ filter (==b) ls