{-# LANGUAGE
   NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import System.Environment
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad

import Debug.Trace

import DSL
import DSL.Lib
import DSL.Compiler
import Parser

import DSL.MemoryManager

import BrainFuckInterpreter

parseAndRun :: String -> IO ()
parseAndRun prog = do
  let comm = parse bf "" prog
  case comm of
    Left e     -> error $ "Error: " ++ show e
    Right prog -> let prog' = commandToProg $ prog
                  in runBFM $ interp prog'

main = do
  let code = encodeString
  prog <- runVarM code
  
  let src = prettySrc prog
  putStrLn $ src
  -- 1276 -- 1234 -- 1194
  putStrLn $ show $ length src
  putStrLn "Go ahead..."
  parseAndRun src