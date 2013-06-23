{-# LANGUAGE
   NoMonomorphismRestriction
 , RankNTypes #-}

module Main where

import DSL
import DSL.Lib
import Parser

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

parseAndExec :: String -> IO ()
parseAndExec prog = do
  case parseBF prog of
    Left e      -> error $ "Parse error: " ++ show e
    Right prog' -> let prog'' = commandToDSL $ prog'
                   in  exec prog''

main :: IO ()
main = do
  let code = encodeString
  exec code