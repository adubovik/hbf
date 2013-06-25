module DSL.MemoryManager(
   mkMemoryMap
 , MemoryMap
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Writer

import Types

import DSL
import DSL.AST.Memory

type MemoryMap   = Map.Map Var Int
type MemoryState = Set.Set Int

type MemM = StateT MemoryState
              (Writer MemoryMap)

-------------------------------------------------
-- Memory managment

newVariable :: MemoryState -> (Var, MemoryState)
newVariable mem = (var, mem')
  where
    cells    = Set.toList mem
    freeCell = head $ filter (not . (`elem` cells)) [0..]
    var      = Var freeCell
    mem'     = Set.insert freeCell mem

delVariable :: Var -> MemoryState -> MemoryState
delVariable (Var cell) mem =
  Set.delete cell mem
delVariable v _ = error $ "delVariable (" ++ show v ++ ") mem"

----------------------

newArray :: Int -> MemoryState -> (Arr Var, MemoryState)
newArray n mem = (arr, mem')
  where
    m = arrLength arr
    arrCands = map (\x -> [x+1..x+m]) ((-1) : Set.toList mem)
    arrCands'= filter (not . any (`Set.member` mem)) arrCands
    arrCells = head arrCands'
    initCell = head arrCells
    arr      = Arr (Var initCell) n
    mem'     = mem <> (Set.fromList arrCells)

delArray :: Arr Var -> MemoryState -> MemoryState
delArray arr@(Arr (Var initCell) _) mem =
  let str = initCell
      end = str + arrLength arr - 1
  in  mem `Set.difference` Set.fromList [str,end]
delArray arr _ = error $ "delArray (" ++ show arr ++ ") mem"

----------------
-- Runner

mkMemoryMap :: Stack () -> MemoryMap
mkMemoryMap = snd . runWriter . flip evalStateT Set.empty .
              foldFree return alg . runStack
  where    
    alg :: ASTF (MemM ()) -> MemM ()
    alg (LocalVar v act rest) = do
      t <- newVar
      registerVar v t
      act
      delVar t
      rest
      
    alg (LocalArr n arr act rest) = do
      t <- newArr n
      registerArr arr t
      act
      delArr t
      rest

    alg (Switch _ _ rest) = rest
    alg Stop = return ()

    registerVar v (Var i) = tell (Map.singleton v i)
    registerVar _ v' = error $ "registerVar v (" ++ show v' ++ ")"
    
    registerArr (Arr v _) (Arr (Var i) _) = tell (Map.singleton v i)
    registerArr a1 a2 = error $ "registerArr " ++ "(" ++ 
                        show a1 ++ ")(" ++ show a2 ++ ")"

    newArr :: Int -> MemM (Arr Var)
    newArr n = do
      mem <- get
      let (arr,mem') = newArray n mem
      put mem'
      return arr

    delArr :: Arr Var -> MemM ()
    delArr arr = do
      modify (delArray arr)

    newVar :: MemM Var
    newVar = do
      mem <- get
      let (var, mem') = newVariable mem
      put mem'
      return var

    delVar :: Var -> MemM ()
    delVar v = do
      modify (delVariable v)
