module DSL.Lib.Arithmetics where

import DSL
import DSL.Lib.Core

-- | v := max(v-n, 0)
abssub :: DSL r => VarD r -> VarD r -> r ()
abssub v n = do
  localVar $ \n' -> do
    n' =: n
    while n' $ do
      n' -= 1
      ifthen v (v -= 1) (return ())

infixl 6 -|
(-|) :: DSL r => VarD r -> VarD r -> r ()
(-|) = abssub

-- | r := n `mod` d
--   q := n `div` d
divmod :: DSL r =>
          VarD r -> VarD r -> VarD r -> VarD r -> r ()
divmod n d r q =
  localVar $ \n' -> do
    localVar $ \d' -> do
      n' =: n
      d' =: d
        
      zero r
      zero q

      while n' $ do
        n' -= 1
        d' -= 1
        r  += 1
        ifthen d' (return ()) $ do
          while r $ do
            r  -= 1
            d' += 1
          q += 1

-- | v := v `div` n
quotient :: DSL r => Int -> VarD r -> r ()
quotient n v = do
  localVar $ \n' -> do
    n' += n
    localVar $ \r -> do
      localVar $ \q -> do
        divmod v n' r q
        v =: q
  
-- | v := v `mod` n
remainder :: DSL r => Int -> VarD r -> r ()
remainder n v = do
  localVar $ \n' -> do
    n' += n
    localVar $ \r -> do
      localVar $ \q -> do
        divmod v n' r q
        v =: r

data Oper = Add | Sub

operVV :: DSL r => Oper -> VarD r -> VarD r -> r ()
operVV op a b = do
  localVar $ \t -> do
    while b $ do
      case op of
        Add -> a += 1
        Sub -> a -= 1
      b -= 1
      t += 1
    while t $ do
      b += 1
      t -= 1

infixl 6 +=:, -=:
(+=:),(-=:), (*=:), (/=:), (%=:) :: DSL r => VarD r -> VarD r -> r ()

-- | a := a + b
a +=: b = operVV Add a b

-- | a := a - b
a -=: b = operVV Sub a b

infixl 7 *=:, /=:

-- | a := a * b
a *=: b = do
  localVar $ \a' -> do
    a +> a'
    while a' $ do
      forV b (a += 1)
      a' -= 1

-- | a := a / b
a /=: b = do
  localVar $ \r -> do
    localVar $ \q -> do
      divmod a b r q
      a =: q

-- | a := a % b
a %=: b = do
  localVar $ \r -> do
    localVar $ \q -> do
      divmod a b r q
      a =: r


-- | (a,b) := (b,a)
swap :: DSL r => VarD r -> VarD r -> r ()
swap a b = do
  localVar $ \t -> do
    a +> t
    b +> a
    t +> b

infixl 4 ===:, =/=:

(===:), (=/=:) :: DSL r => VarD r -> VarD r -> r ()

-- | a := (a == b) ? 1 : 0
a ===: b = do
  localVar $ \d1 -> do
    localVar $ \d2 -> do
      d1 =: a
      d2 =: b
      d1 -| b
      d2 -| a

      zero a
      d1 +> a
      d2 +> a

      ifthen a (zero a) (a += 1)

-- | a := (a /= b) ? 1 : 0
a =/=: b = do
  a ===: b
  neg a

-- | a := if a==0 then 0 else 1
pos :: DSL r => VarD r -> r ()
pos a = do
  ifthen a (zero a >> a += 1) (return ())

-- | a := if a==0 then 1 else 0
neg :: DSL r => VarD r -> r ()
neg a = do
  ifthen a (zero a) (zero a >> a += 1)

infixl 4 =<:, =>:
(=<:), (=>:), (=<=:), (=>=:) :: DSL r => VarD r -> VarD r -> r ()

-- | a := a < b
a =<: b = do
  localVar $ \d1 -> do
    d1 =: a
    d1 -| b
    neg d1
    localVar $ \d2 -> do
      d2 =: b
      d2 -| a
      pos d2
      
      zero a
      ifthen d1 (
        ifthen d2
          (a += 1)
          (return ()))
        (return ())

-- | a := a > b
(=>:) = flip (=<:)

-- | a := a <= b
a =<=: b = do
  localVar $ \eq -> do
    eq =: a
    eq ===: b
    localVar $ \ls -> do
      ls =: a
      ls =<: b

      zero a
      ifthen eq (a += 1)
        (ifthen ls (a += 1)
           (return ()))

-- | a := a >= b
(=>=:) = flip (=<=:)