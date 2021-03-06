{-# language
   TypeSynonymInstances
 , FlexibleInstances
 , FlexibleContexts
 , DeriveTraversable
 , DeriveFoldable
 , DeriveFunctor
 , TypeFamilies
 , MultiParamTypeClasses
 , UndecidableInstances
 , NoMonomorphismRestriction #-}

module DSL.AST.Base
  ( ASTF(..)
  , ArOp(..)
  , IOOp(..)
  , AST
  , runStack
  )
where

import qualified Data.Map as Map
import Data.Traversable
import Data.Foldable
import Data.Maybe

import Control.Arrow hiding(arr)
import Control.Applicative
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Reader

import Types hiding (Command(..))

import DSL
import DSL.Lib

import DSL.MemoryManager

data ArOp = Inc | Dec deriving Show
data IOOp = Put | Get deriving Show

data ASTF r = Arith ArOp r
            | InOut IOOp r
            | While r r
            | Switch Int r
            | Stop
            deriving (Functor, Traversable, Foldable, Show)

type AST = Free ASTF
type Stack = FreeT ASTF
               (StateT (Var,Int)
                 (Reader MemoryMap))

-- liftF :: Functor f => f a -> Free f ()
-- liftF = Impure . fmap (const $ return ())

stop :: MonadFree ASTF m => m a
stop = wrap Stop

stopped :: MonadFree ASTF m => m a -> m b
stopped act = act >>= const stop

instance DSL Stack where
  type VarD Stack = Var
  
  localVar act = do
    idx <- snd <$> get
    modify (second (+1))
    let var = Var idx
    act var
    -- FIXME: check out if variable needed later
    zero var

  localArr n act = do
    idx <- snd <$> get
    modify (second (+1))
    let arr = Arr (Var idx) n
    act arr
    -- FIXME: check out if array needed later
    zeroArray arr

  switch v1 = do
    v2 <- fst <$> get
    modify (first $ const v1)
    mem <- ask
    
    let Var i1 = normVar (mapVar findIdx v1)
        Var i2 = normVar (mapVar findIdx v2)
        findIdx i = fromMaybe
                    (error $ "Can't find var: " ++ show i)
                    (Map.lookup (Var i) mem)
    when (i1 /= i2) $ do
      wrap $ Switch (i1 - i2) (trans $ return ())
    return ()

  incU = wrap $ Arith Inc (trans $ return ())
  decU = wrap $ Arith Dec (trans $ return ())

  putcharU = wrap $ InOut Put (trans $ return ())
  getcharU = wrap $ InOut Get (trans $ return ())

  predU = wrap $ Switch (-1) (trans $ return ())
  succU = wrap $ Switch ( 1) (trans $ return ())

  whileU act = wrap $ While (stopped act) (trans $ return ())

runStack :: Stack a -> MemoryMap -> AST a
runStack st mem = flip runReader mem $
                  evalStateT (untrans st) (Var 0, 0)

