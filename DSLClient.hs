{-# LANGUAGE
   NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import System.Environment
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad
import Text.JSON

import Debug.Trace

import DSL
import DSL.Lib
import DSL.Compiler
import Parser

import DSL.MemoryManager

import BrainFuckInterpreter
--import StringInterpreter
--import IOInterpreter

-- for IO
parseAndRun :: String -> IO ()
parseAndRun prog = do
  let comm = parse bf "" prog
  case comm of
    Left e     -> error $ "Error: " ++ show e
    Right prog -> let prog' = commandToProg $ prog
                  in runBFM $ interp prog'

-- for IO
main' = do
  args <- getArgs
  inp <- if (length args >= 1)
         then readFile (args !! 0)
         else getContents
  let src = filter (`elem` "[]<>+-.,") inp
  parseAndRun src

prog1 = do
  a <- newVar
  getchar a
  a +: (- (ord '0'))
  div2 a
  a +: (ord '0')
  putchar a

prog2 = do
  let n = 10
  arr <- mkArr n
  x <- newVar
  c <- newVar
  
  repeatCode n $ do
    getchar c
    setArrayCell x c arr
    x +: 1
  
  repeatCode n $ do
    x +: (-1)
    getArrayCell c x arr
    putchar c

main = do
  let code = repeatCode 26 (encodeChar 5)
  prog <- runVarM code
  
  let src = prettySrc prog
  putStrLn $ src
  -- 1276 -- 1234
  putStrLn $ show $ length src
  putStrLn "Go ahead..."
  --parseAndRun src

  putStrLn $
    (encode :: JSValue -> String) $ 
    prettyProg $ runPointerM code

-- -- for String
-- main = do
--   prog <- runVarM prog1
--   let src = prettySrc prog
--   --putStrLn $ prettySrcDense prog
--   putStrLn "Go ahead..."
--   input <- getLine
--   putStr $ parseAndRun' src input

-- -- for String
-- parseAndRun' prog input = do
--   let comm = parse bf "" prog
--   case comm of
--     Left e     -> error $ "Error: " ++ show e
--     Right prog -> let prog' = commandToProg $ prog
--                   in runBFM (interp prog') input
