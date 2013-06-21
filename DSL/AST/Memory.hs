{-# language
   TypeSynonymInstances
 , FlexibleInstances
 , DeriveTraversable
 , DeriveFoldable
 , DeriveFunctor
 , TypeFamilies
 , MultiParamTypeClasses
 , UndecidableInstances
 , NoMonomorphismRestriction #-}

module DSL.AST.Memory where

import Data.Traversable
import Data.Foldable

import Control.Arrow
import Control.Applicative
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Free.Instances

import Types
import DSL
import DSL.Lib

data ASTF r = LocalVar     Var r r
            | LocalArr Int (Arr Var) r r
            | Switch Var Var r
            | Stop
            deriving (Functor, Traversable, Foldable, Show)

type AST   = Free ASTF
type Stack = FreeT ASTF
               (State (Var, Int))

liftF :: Functor f => f a -> Free f ()
liftF = Impure . fmap (const $ return ())

stop = wrap Stop
stopped act = act >>= const stop

instance DSL Stack where
  type VarD Stack = Var
  
  localVar act = do
    idx <- snd <$> get
    modify (second (+1))
    let var = Var idx
    wrap $ LocalVar var (stopped $ act var) (trans $ return ())

  localArr n act = do
    idx <- snd <$> get
    modify (second (+1))
    let arr = Arr (Var idx) n
    wrap $ LocalArr n arr (stopped $ act arr) (trans $ return ())

  switch v1 = do
    v2 <- fst <$> get
    modify (first $ const v1)
    when (v1 /= v2) $ wrap $ Switch v2 v1 (trans $ return ())
  
  incU = return ()
  decU = return ()
  predU = return ()
  succU = return ()
  putcharU = return ()
  getcharU = return ()
  
  whileU act = act

runStack :: Stack a -> AST a
runStack st = evalState (untrans st) (Var 0, 0)

