module DSL.PrettyPrinter
  ( pprintAST
  )
where

import DSL.AST.Base
import Control.Monad.Free
  
pprintAST :: AST () -> String
pprintAST = foldFree (const "") pp
  where
    pp (Arith op r) = ppAr op ++ r
      where
        ppAr Inc = "+"
        ppAr Dec = "-"
    pp (InOut op r) = ppIo op ++ r
      where
        ppIo Put = "."
        ppIo Get = ","
    pp (While a r) = "[" ++ a ++ "]" ++ r
    pp (Switch i r) = replicate (abs i) c ++ r
      where
        c | i<=0      = '<'
          | otherwise = '>'
    pp Stop = ""
  