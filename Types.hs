module Types(Command(..), optBF) where

data Command = Succ | Pred |
               Inc | Dec |
               PutChar | GetChar |
               While [Command]
               deriving (Show, Eq)

optBF :: [Command] -> [Command]
optBF xs = optReduce (Succ,Pred) .
           optReduce (Inc,Dec) .
           subOpt $ xs
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