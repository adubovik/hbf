{-# language
   RankNTypes
 #-}

module DSL.Compiler
  ( compile
  , Some(..)
  )
where

import DSL
import DSL.MemoryManager
import DSL.AST.Base

data Some = Some (forall r . DSL r => r ())

compile :: Some -> AST ()
compile (Some prog) = runStack prog (mkMemoryMap prog)
