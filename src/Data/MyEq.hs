module Data.MyEq where

import Data.Generics

class MyEq a where

  myEq :: a -> a -> Bool

  default myEq :: (Generic a, GMyEq (Rep a)) => a -> a -> Bool
  myEq a b = gMyEq (from a) (from b)

class GMyEq a where

  gMyEq :: a -> a -> Bool

instance (GMyEq a, GMyEq b) => GMyEq (a :+: b) where
  gMyEq (L l1) (L l2) = gMyEq l1 l2
  gMyEq (R r1) (R r2) = gMyEq r1 r2
  gMyEq _ _ = False

instance (GMyEq a, GMyEq b) => GMyEq (a :*: b) where
  gMyEq (a1 :*: b1) (a2 :*: b2) = gMyEq a1 a2 && gMyEq b1 b2

instance MyEq a => GMyEq (V a) where
  gMyEq a b = myEq (unV a) (unV b)

instance MyEq a => GMyEq (Rec a) where
  gMyEq a b = myEq (unRec a) (unRec b)

instance GMyEq U where
  gMyEq U U = True
