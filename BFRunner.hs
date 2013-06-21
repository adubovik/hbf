{-# LANGUAGE
   NoMonomorphismRestriction
 , RankNTypes #-}

module Main where

import Data.Char
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Applicative
import Control.Monad

import DSL
import DSL.Lib
import Parser

import DSL.AST.Base
import DSL.Compiler
import DSL.MemoryManager
import DSL.Interpreter

exec :: (forall r . DSL r => r ()) -> IO ()
exec prog = do
  let bf    = compile (Some prog)
      bfSrc = pprintAST bf
  print $ "Brainfuck source code [" ++ show (length bfSrc) ++ "]"
  putStrLn bfSrc
  print "Memory map:"
  putStrLn $ show $ mkMemoryMap prog
  print "Interpreting, go ahead..."
  runIOBF (interp bf)

parseAndExec :: String -> IO ()
parseAndExec prog = do
  let comm = parse bf "" prog
  case comm of
    Left e     -> error $ "Parse error: " ++ show e
    Right prog -> let prog' = commandToDSL $ prog
                  in exec prog'

main = do
  let code = encodeString
  exec code