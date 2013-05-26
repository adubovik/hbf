{-# LANGUAGE
   DeriveFunctor #-}

module FreeInterpreter where

----------------------------

data Free r a = Free { unFree :: r (Free r a) } | Open a

instance Functor r => Functor (Free r) where
  fmap f (Open a) = Open $ f a
  fmap f (Free r) = Free $ fmap (fmap f) r

instance Functor r => Monad (Free r) where
  return  = Open
  (Open a) >>= f = f a
  (Free x) >>= f = Free $ fmap (>>= f) x

data BF r = Succ r |
            Pred r |
            Inc r |
            Dec r |
            PutChar r |
            GetChar r |
            While (Free BF ()) r
            deriving (Functor)

prettyShallow :: Free BF () -> String
prettyShallow (Open _) = ""
prettyShallow (Free x) = pretty x
  where
    pretty (Succ _) = "Succ"
    pretty (Pred _) = "Pred"
    pretty (Inc _) = "Inc"
    pretty (Dec _) = "Dec"
    pretty (PutChar _) = "PutChar"
    pretty (GetChar _) = "GetChar"
    pretty (While a _) = "While[" ++ prettyShallow a ++ "]"

wrap :: Functor f => f a -> Free f a
wrap = Free . fmap return

succ', pred', inc, dec, putchar, getchar :: Free BF ()
succ' = wrap $ Succ ()
pred' = wrap $ Pred ()
inc = wrap $ Inc ()
dec = wrap $ Dec ()
putchar = wrap $ PutChar ()
getchar = wrap $ GetChar ()

while :: Free BF () -> Free BF ()
while bf = wrap $ While bf ()

--------------------------------
-- Interpreters

prettyBF :: Free BF () -> String
prettyBF (Open ()) = ""
prettyBF (Free bf) = case bf of
  Succ r -> ">" ++ prettyBF r
  Pred r -> "<" ++ prettyBF r
  Inc  r -> "+" ++ prettyBF r
  Dec  r -> "-" ++ prettyBF r
  PutChar r -> "." ++ prettyBF r
  GetChar r -> "," ++ prettyBF r
  While r1 r2 -> "[" ++ prettyBF r1 ++ "]" ++ prettyBF r2