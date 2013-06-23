{-# LANGUAGE
   NoMonomorphismRestriction
 , RankNTypes #-}

module Main where

import DSL
import DSL.Lib
import Parser

import DSL.Compiler
import DSL.Interpreter

execIO :: (forall r . DSL r => r ()) -> IO ()
execIO prog = do
  putStrLn "Interpreting, go ahead..."
  runIOBF . interp . compile $ Some prog

parseAndExec :: String -> IO ()
parseAndExec prog = do
  case parseBF prog of
    Left e      -> error $ "Parse error: " ++ show e
    Right prog' -> let prog'' = commandToDSL prog'
                   in  execIO prog''

main :: IO ()
main = do
  code <- getContents
  parseAndExec code