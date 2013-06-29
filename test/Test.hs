{-# LANGUAGE
   NoMonomorphismRestriction
 , ExistentialQuantification
 , RankNTypes
 , ScopedTypeVariables #-}

module Main(main) where

import Data.Char
import Test.HUnit
import Text.Printf

import DSL
import DSLCont
import DSL.Lib

import DSL.Compiler
import DSL.Interpreter

(~=?=) :: (Eq b, Show b) => b -> b -> Test
(~=?=) = flip (~=?)

runOn :: (forall r . DSL r => r ()) -> (String -> String)
runOn prog = runDetBF (interp $ compile (Some prog))

constNil :: Test
constNil = test (map mkTest testSrc)
  where
    testSrc = "ah0"
    mkTest c = [c] ~: t [c] ~=?= "0"
    
    t = runOn prog
    prog =
      localVar $ \a -> do
        getchar a
        zero a
        a += ord '0'
        putchar a

nextCharTest :: Test
nextCharTest = test (map mkTest testSrc)
  where
    testSrc = ['a'..'f']
    mkTest c = [c] ~: t [c] ~=?= [chr (ord c + 1)]
    
    t = runOn prog
    prog =
      localVar $ \a -> do
        getchar a
        a += 1
        putchar a

binaryOutTest :: Test
binaryOutTest = test $ map mkTest "abcdefxyz"
  where
    m = 5
    mkTest c = ("binout_" ++ [c]) ~: t [c] ~=?= (brute c ++ "\n")
    brute :: Char -> String
    brute c = go m c'
      where
        c' = ord c - ord 'a'
        go :: Int -> Int -> String
        go 0 _  = ""
        go n ch = go (n-1) (ch `div` 2) ++
                 [if even ch then '-' else '*']
    
    t = runOn prog
    prog = encodeChar m

abssubTest :: Test
abssubTest = test (map mkTest testSrc)
  where
    testSrc :: [(Int,Int)] = 
      [ (2,3),(3,1),(4,7),(9,4)
      , (9,8),(0,1),(1,0),(0,0)
      ]
    
    mkTest (a,b) = (show a ++ show b) ~: t (show a ++ show b) ~=?= show (max (a-b) 0)

    t = runOn prog
    prog =
      localVar $ \a -> do
        getchar a
        localVar $ \b -> do
          getchar b

          a -= ord '0'
          b -= ord '0'
          a -| b
          a += ord '0'

          putchar a

copyTest :: Test
copyTest = test (map mkTest testSrc)
  where
    testSrc :: [Int] = [2,3,4,9,8]
    mkTest n = (show n) ~: t (show n) ~=?= (show n ++ show n)
    
    t = runOn prog
    prog = do
      localVar $ \a -> do
        getchar a
        localVar $ \b -> do  
          b =: a
          putchar b
          putchar a

addTest :: Test
addTest = test (map mkTest testSrc)
  where
    -- only one digit numbers in `testSrc`!
    testSrc :: [(Int,Int)] = [(2,3),(4,5),(5,4),(0,0),(0,1),(1,0)]
    mkTest (a,b) = (show a ++ show b) ~: t (show a ++ show b) ~=?= ("0" ++ show (a+b))
    
    t = runOn prog
    prog = do
      localVar $ \a -> do
        getchar a
        a -= ord '0'
        localVar $ \b -> do          
          getchar b
          b -= ord '0'

          add a b

          a += ord '0'
          b += ord '0'
          putchar a
          putchar b

reverseStringTest :: Test
reverseStringTest = test (map mkTest testSrc)
  where
    testSrc = ["","1","12","123","1234567890"]
    mkTest str = str ~: t (length str) str ~=?= (reverse str)
    
    t n = runOn (prog n)
    prog n = localArr n $ \arr -> do
      localVar $ \x -> do
        localVar $ \c -> do
          forI n $ do
            getchar c
            setArrayCell x c arr
            x += 1

          forI n $ do
            x -= 1
            getArrayCell c x arr
            putchar c

divModTest :: Test
divModTest = test (map mkTest testSrc)
  where
    testSrc :: [(Int,Int)] = 
      [ (1,2),(2,1),(9,2),(9,5)
      , (7,3),(0,1),(13,4),(15,3)
      , (200,7),(134,110)
      ]
    mkTest (n,d) = (show n ++ " `divMod` " ++ show d) ~:
                   t (show n ++ " " ++ show d ++ " ") ~=?=
                   (show (n `div` d) ++ " " ++ show (n `mod` d))
    
    t = runOn prog
    prog =
      localVar $ \n -> do
        localVar $ \d -> do
          readInt n          
          readInt d

          localVar $ \r -> do
            localVar $ \q -> do
              divmod n d r q

              printInt q
              printChar ' '
              printInt r

dslContTest :: Test
dslContTest = test (map mkTest testSrc)
  where
    testSrc :: [(Int,Int,Int)] = 
      [(1,2,3),(5,0,6),(0,0,0),(7,7,7),(1,1,1),(11,0,9)]
    mkTest (x,y,z) = 
      show [x,y,z] ~: t (printf "%d %d %d " x y z) ~=?= 
                        (printf "%d %d" 
                          ((x*x + y*y + z*z) `div` 3)
                          ((x + y + z) `div` 3))
    
    t = runOn prog

    prog = do
      localVar $ \x -> do
        readInt x
        localVar $ \y -> do
          readInt y
          localVar $ \z -> do
            readInt z
            printInt <!> 
              ((u x * u x) + (u y * u y) + (u z * u z)) `div` 3
            printChar ' '
            printInt <!> (u x + u y + u z) `div` 3


tests :: Test
tests = test [ "const"          ~: constNil
             , "inc"            ~: nextCharTest
             , "binout"         ~: binaryOutTest
             , "safeSub"        ~: abssubTest
             , "copy"           ~: copyTest
             , "add"            ~: addTest
             , "reverse string" ~: reverseStringTest
             , "divmod"         ~: divModTest
             , "dslCont"        ~: dslContTest
             ]

main :: IO ()
main = runTestTT tests >> return ()