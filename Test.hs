{-# LANGUAGE
   NoMonomorphismRestriction #-}

module Test(runTests) where

import Data.Char
import Control.Monad
import Control.Monad.Identity
import Test.HUnit

import DSL
import DSL.Lib
import DSL.Compiler

import StringInterpreter
import Types

(~=?=) = flip (~=?)

runOn :: VarM Identity () -> (String -> String)
runOn prog input = runBFM (interp $ commandToProg code) input
    where
      code :: [Command]
      code = runIdentity $ runVarM prog

test1 = test
        ["const0_a" ~: t "a" ~=?= "0",
         "const0_h" ~: t "h" ~=?= "0",
         "const0_0" ~: t "0" ~=?= "0"]
  where
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog = do
      a <- newVar
      getchar a
      zero a
      a +: (ord '0')
      putchar a

test2 = test
        ["inc_a" ~: t "a" ~=?= "b",
         "inc_i" ~: t "i" ~=?= "j",
         "inc_5" ~: t "5" ~=?= "6"]
  where
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog = do
      a <- newVar
      getchar a
      a +: 1
      putchar a

test3 = test
        ["div2_2" ~: t "2" ~=?= "1",
         "div2_3" ~: t "3" ~=?= "1",
         "div2_0" ~: t "0" ~=?= "0",
         "div2_9" ~: t "9" ~=?= "4"]
  where
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog = do
      a <- newVar
      getchar a
      a +: (- (ord '0'))
      div2 a
      a +: (ord '0')
      putchar a

test4 = test
        ["mod2_2" ~: t "2" ~=?= "0",
         "mod2_3" ~: t "3" ~=?= "1",
         "mod2_0" ~: t "0" ~=?= "0",
         "mod2_9" ~: t "9" ~=?= "1"]
  where
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog = do
      a <- newVar
      getchar a
      a +: (- (ord '0'))
      mod2 a
      a +: (ord '0')
      putchar a

brute c = go 5 c'
  where
    c' = ord c - ord 'a'
    go 0 c = ""
    go n c = go (n-1) (c `div` 2) ++
             [if even c then '-' else '*']

test5 = test $ map mkTest "abcdefxyz"
  where
    m = 5
    mkTest c = ("binout_" ++ [c]) ~:
                 t [c] ~=?= (brute c ++ "\n")
    brute c = go m c'
      where
        c' = ord c - ord 'a'
        go 0 c = ""
        go n c = go (n-1) (c `div` 2) ++
                 [if even c then '-' else '*']
    
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog = encodeChar m

test6 = test
        ["23" ~: t "23" ~=?= "0",
         "31" ~: t "31" ~=?= "2",
         "47" ~: t "47" ~=?= "0",
         "94" ~: t "94" ~=?= "5",
         "98" ~: t "98" ~=?= "1"]
  where
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog = do
      a <- newVar
      getchar a
    
      b <- newVar
      getchar b

      a +: (- (ord '0'))
      b +: (- (ord '0'))

      a -| b

      a +: (ord '0')

      putchar a

test7 = test
        ["2" ~: t "2" ~=?= "22",
         "3" ~: t "3" ~=?= "33",
         "4" ~: t "4" ~=?= "44",
         "9" ~: t "9" ~=?= "99",
         "8" ~: t "8" ~=?= "88"]
  where
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog = do
      a <- newVar
      getchar a
  
      b <- newVar
      copy a b

      putchar b
      putchar a

test8 = test
        ["23" ~: t "23" ~=?= "05",
         "45" ~: t "45" ~=?= "09",
         "54" ~: t "54" ~=?= "09",
         "00" ~: t "00" ~=?= "00",
         "01" ~: t "01" ~=?= "01",
         "10" ~: t "10" ~=?= "01"]
  where
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog = do
      a <- newVar
      getchar a
      a +: (- (ord '0'))
  
      b <- newVar
      getchar b
      b +: (- (ord '0'))

      add a b
  
      a +: (ord '0')
      b +: (ord '0')
      putchar a
      putchar b

test9 = test
        [ "0"  ~: t 0 ""            ~=?= ""
        , "1"  ~: t 1 "1"           ~=?= "1"
        , "2"  ~: t 2 "12"          ~=?= "21"
        , "3"  ~: t 3 "123"         ~=?= "321"
        , "10" ~: t 10 "1234567890" ~=?= "0987654321"
        ]
  where
    t n = runOn (prog n)
    prog :: Monad m => Int -> VarM m ()
    prog n = localArr n $ \arr -> do
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

test10 = test
        [ "12"  ~: t "12" ~=?= "10"
        , "21"  ~: t "21" ~=?= "02"
        , "92"  ~: t "92" ~=?= "14"
        , "95"  ~: t "95" ~=?= "41"
        , "73"  ~: t "73" ~=?= "12"
        ]
  where
    t = runOn prog
    prog :: Monad m => VarM m ()
    prog =
      localVar $ \n -> do
        localVar $ \d -> do
          getchar n
          n +: (- (ord '0'))
          
          getchar d
          d +: (- (ord '0'))

          localVar $ \r -> do
            localVar $ \q -> do
              divmod n d r q

              r +: (ord '0')
              putchar r
              
              q +: (ord '0')
              putchar q

tests = test [ "const"   ~: test1
             , "inc"     ~: test2
             , "div2"    ~: test3
             , "mod2"    ~: test4
             , "binout"  ~: test5
             , "safeSub" ~: test6
             , "copy"    ~: test7
             , "add"     ~: test8
             , "reverseStringArray" ~: test9
             , "divmod" ~: test10
             ]

runTests = runTestTT tests