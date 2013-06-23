{-# LANGUAGE
   TypeSynonymInstances
 , FlexibleInstances
 , GeneralizedNewtypeDeriving
 , ViewPatterns #-}

module NestedStructure
  ( NestedStructure(..)
  , MonoidNest(..)
  )
where

import Text.JSON
import Data.Monoid
import Data.List

class NestedStructure a where
  nullNode :: a
  appendSibling :: a -> a -> a
  node :: String -> a -> a

newtype MonoidNest a = MonoidNest { unMonoidNest :: a }
                     deriving NestedStructure

instance NestedStructure a => Monoid (MonoidNest a) where
  mempty  = nullNode
  mappend = appendSibling

instance NestedStructure String where
  nullNode = mempty
  appendSibling = mappend
  node val child = intercalate "\n" $ (val:) $ map ("  "++) $ lines child

-- Use awesome app jsoneditoronline.org to explore the structure
instance NestedStructure JSValue where
  nullNode = JSNull
  appendSibling JSNull x = x
  appendSibling x JSNull = x
  appendSibling (JSObject (fromJSObject -> lst1))
                (JSObject (fromJSObject -> lst2)) =
    JSObject $ toJSObject (append lst1 lst2)
    where
      append [] x = x
      append x [] = x
      append lst1 lst2
        | tpx == JSNull && tpy == JSNull = init lst1 ++ [(x++y,JSNull)] ++ tail lst2
        | otherwise = lst1 ++ lst2
        where
          (x,tpx) = last lst1
          (y,tpy) = head lst2
  appendSibling x y = error $ show x ++ " & " ++ show y
  node val child = JSObject $ toJSObject [(val, child)]
