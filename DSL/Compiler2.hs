{-# language
   TypeSynonymInstances
 , FlexibleInstances
 , DeriveTraversable
 , DeriveFoldable
 , DeriveFunctor
 , TypeFamilies
 , MultiParamTypeClasses
 , UndecidableInstances
 , NoMonomorphismRestriction
 , RankNTypes
 , ExistentialQuantification #-}

module DSL.Compiler2 where

import DSL
import DSL.Lib
import DSL.MemoryManager
import DSL.MainAST

data Some = Some (forall r . DSL r => r ())

compile :: Some -> AST ()
compile (Some prog) = runStack prog (mkMemoryMap prog)
