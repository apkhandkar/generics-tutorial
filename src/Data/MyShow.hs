module Data.MyShow where

import Data.Generics
import GHC.TypeLits
import Data.Proxy

class MyShow a where

  myShow :: a -> String

  default myShow :: (Generic a, GMyShow (Rep a)) => a -> String
  myShow = gMyShow . from

class GMyShow a where
  
  gMyShow :: a -> String

instance (GMyShow a, GMyShow b) => GMyShow (a :+: b) where
  gMyShow (L l) = gMyShow l
  gMyShow (R r) = gMyShow r

instance (GMyShow a, GMyShow b) => GMyShow (a :*: b) where
  gMyShow (a :*: b) = gMyShow a <> " " <> gMyShow b

instance (KnownSymbol n, GMyShow a) => GMyShow (C n f a) where
  gMyShow a = symbolVal (Proxy @n) <> " " <> gMyShow (unC a)

instance (KnownSymbol n, GMyShow a) => GMyShow (S n a) where
  gMyShow a = symbolVal (Proxy @n) <> ":" <> gMyShow (unS a)

instance MyShow a => GMyShow (V a) where
  gMyShow a = myShow (unV a)

instance MyShow a => GMyShow (Rec a) where
  gMyShow a = myShow (unRec a)

instance GMyShow U where
  gMyShow U = ""

instance GMyShow a => GMyShow (M n m p nt a) where
  gMyShow a = gMyShow (unM a)

-- ** Auto-derive instances for common types

instance MyShow Bool

instance MyShow a => MyShow [a]
