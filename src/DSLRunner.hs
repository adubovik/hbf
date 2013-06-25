{-# LANGUAGE
   NoMonomorphismRestriction
 , RankNTypes #-}

module Main where

import DSL
import DSL.Lib.EncodeString

import DSL.PrettyPrinter
import DSL.Compiler
import DSL.MemoryManager
import DSL.Interpreter

exec :: (forall r . DSL r => r ()) -> IO ()
exec prog = do
  let bf    = compile (Some prog)
      bfSrc = pprintAST bf
      
  putStrLn $ "Brainfuck source code [" ++ show (length bfSrc) ++ "]"
  putStrLn bfSrc
  
  putStrLn "Memory map:"
  putStrLn $ show $ mkMemoryMap prog
  
  putStrLn "Interpreting, go ahead..."
  runIOBF (interp bf)

main :: IO ()
main = do
  let code = encodeString
  exec code