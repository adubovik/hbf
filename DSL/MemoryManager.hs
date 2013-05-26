{-# LANGUAGE
   TypeFamilies
 , TypeSynonymInstances
 , FlexibleInstances
 , GeneralizedNewtypeDeriving
 , ViewPatterns #-}

module DSL.MemoryManager where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Arrow
import Control.Applicative

import Data.List

import DSL
import DSL.Lib
import NestedStructure

type Prog = [Command]

data Command = Inc Var
             | Dec Var
             | PutChar Var
             | GetChar Var
             | SetArr { cidx :: Var, cval :: Var, carr :: Array }
             | GetArr { cidx :: Var, cval :: Var, carr :: Array }
             | While Var Prog
             deriving Show

prettyVar i' (Var ann i)
  | i' == i   = ""
  | otherwise = maybe "" (++".") ann ++ show i
prettyArr i' (Array ann _ i)
  | i' == i   = ""
  | otherwise = maybe "" (++".") ann ++ show i

tellNest s = tell $ node s nullNode

prettyCommandM :: NestedStructure a => Command -> StateT (Int, Id) (Writer (MonoidNest a)) ()
prettyCommandM (Inc v@(Var _ i)) = do
  (d,i') <- get
  put (d,i)
  tellNest $ prettyVar i' v ++ "+"
prettyCommandM (Dec v@(Var _ i)) = do
  (d,i') <- get
  put (d,i)
  tellNest $ prettyVar i' v ++ "-"
prettyCommandM (PutChar v@(Var _ i)) = do
  (d,i') <- get
  put (d,i)
  tellNest $ prettyVar i' v ++ "."
prettyCommandM (GetChar v@(Var _ i)) = do
  (d,i') <- get
  put (d,i)
  tellNest $ prettyVar i' v ++ ","

prettyCommandM (SetArr idx val arr@(Array _ _ i)) = do
  (d,i') <- get
  put (d,i)
  tellNest $ "("  ++ prettyArr (-1) arr ++ "[" ++ prettyVar (-1) idx ++ "]" ++
             ":=" ++ prettyVar (-1) val ++ ")"
             
prettyCommandM (GetArr idx val arr@(Array _ _ i)) = do
  (d,i') <- get
  put (d,i)
  tellNest $ "(" ++ prettyVar (-1) val ++ ":=" ++ prettyArr (-1) arr ++
             "[" ++ prettyVar (-1) idx ++ "]" ++ ")"
             
prettyCommandM (While v@(Var _ i) prog) = do
  (d,i') <- get
  put (d,-1)
  let addTab s = intercalate "\n" $ map ("  " ++) $ lines s
  pass $ do
    prettyProgM prog
    return ((), node $ prettyVar (-1) v)
  put (d,i)
  
prettyProgM :: NestedStructure a => [Command] -> StateT (Int, Id) (Writer (MonoidNest a)) ()
prettyProgM = mapM_ prettyCommandM

prettyCommand :: NestedStructure a => Command -> a
prettyCommand comm = unMonoidNest $ snd $ runWriter $ evalStateT (prettyCommandM comm) (0,-1)

prettyProg :: NestedStructure a => Prog -> a
prettyProg prog = unMonoidNest $ snd $ runWriter $ evalStateT (prettyProgM prog) (0,-1)

type PointerM m = StateT Int
                   (WriterT Prog m)
type Id    = Int
data Var   = Var   (Maybe String)     Id deriving Show
data Array = Array (Maybe String) Int Id deriving Show

instance Monad m => DSL (PointerM m) where
  type   VarD (PointerM m) = Var
  type ArrayD (PointerM m) = Array

  localVar' ann f = do
    let newVar = do
          idx <- get
          modify (+1)
          let lstToMaybe [] = Nothing
              lstToMaybe x  = Just x
          return (Var (lstToMaybe ann) idx)
    x <- newVar
    f x

  localArr' ann n f = do
    let newArr n = do
          idx <- get
          modify (+1)
          let lstToMaybe [] = Nothing
              lstToMaybe x  = Just x
          return (Array (lstToMaybe ann) n 0)
    x <- newArr n
    f x

  localVar = localVar' ""
  localArr = localArr' ""

  inc v = tell [Inc v]
  dec v = tell [Dec v]
  putchar v = tell [PutChar v]
  getchar v = tell [GetChar v]

  -- FIX ME: fix switches considering actual array implementation
  setArrayCell idx val arr = tell [SetArr idx val arr]
  getArrayCell val idx arr = tell [GetArr idx val arr]

  while v act =
    pass $ do
      act
      return ((), return . While v)

-- Harness --

runPointerM :: PointerM Identity () -> Prog
runPointerM act = snd $ runIdentity $ runWriterT $ evalStateT act 0