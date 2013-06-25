module DSL.Lib.Array where

import Control.Monad

import DSL
import DSL.Lib

-- Look at
-- http://esolangs.org/wiki/brainfuck_algorithms#x.28y.29_.3D_z_.281-d_array.29_.282_cells.2Farray_element.29
-- for reference
-- 
-- arr(idx) = src
-- FIXME: handle array out of bounds error
-- idx -> src -> arr
setArrayCell :: DSL r => VarD r -> VarD r -> ArrayD r -> r ()
setArrayCell idx src arr = do
  zero $ arrT0 arr
  zero $ arrT1 arr
  arrT1 arr =: idx
  arrT0 arr =: src

  switch $ arrInit arr

  unsafeBF ">>"
  unsafeBF "[[>>]+[<<]>>-]"
  unsafeBF "+[>>]<[-]<[<<]"
  unsafeBF ">[>[>>]<+<[<<]>-]"
  unsafeBF ">[>>]<<[-<<]"

-- http://esolangs.org/wiki/brainfuck_algorithms#x_.3D_y.28z.29_.281-d_array.29_.282_cells.2Farray_element.29
-- dest = arr(idx)          
-- dst -> idx -> arr
getArrayCell :: DSL r => VarD r -> VarD r -> ArrayD r -> r ()
getArrayCell dst idx arr = do
  zero dst
  zero $ arrT0 arr
  zero $ arrT1 arr
  arrT1 arr =: idx

  switch $ arrInit arr

  unsafeBF ">>"
  unsafeBF "[[>>]+[<<]>>-]"
  unsafeBF "+[>>]<"

  whileU $ do
    unsafeBF "<[<<]>+<"
    switch dst
    incU
    switch $ arrInit arr
    unsafeBF ">>[>>]<-"

  unsafeBF "<[<<]>"
  unsafeBF "[>[>>]<+<[<<]>-]"
  unsafeBF ">[>>]<<[-<<]"

zeroArray :: DSL r => ArrayD r -> r ()
zeroArray arr = do
  switch (arrInit arr)
  replicateM_ (arrLength arr) $ do
    zeroUnsafe
    succU
  replicateM_ (arrLength arr) $ do
    predU
  where
    zeroUnsafe :: DSL r => r ()
    zeroUnsafe = whileU decU

