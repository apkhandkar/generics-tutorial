module Data.TypeInfo where

import Data.Generics
import GHC.TypeLits
import Data.Proxy

data TypeInfo = TypeInfo
  { typeName :: String
  , moduleName :: String
  , packageName :: String
  , isNewtype :: Bool
  } deriving Show

class HasTypeInfo a where

  getTypeInfo :: a -> TypeInfo

  default getTypeInfo :: (Generic a, GHasTypeInfo (Rep a)) => a -> TypeInfo
  getTypeInfo = gGetTypeInfo . from

class GHasTypeInfo a where

  gGetTypeInfo :: a -> TypeInfo

instance (KnownSymbol n, KnownSymbol m, KnownSymbol p, GHasTypeInfo a) => GHasTypeInfo (M n m p True a) where
  gGetTypeInfo _ = TypeInfo (symbolVal (Proxy @n)) (symbolVal (Proxy @m)) (symbolVal (Proxy @p)) True

instance (KnownSymbol n, KnownSymbol m, KnownSymbol p, GHasTypeInfo a) => GHasTypeInfo (M n m p False a) where
  gGetTypeInfo _ = TypeInfo (symbolVal (Proxy @n)) (symbolVal (Proxy @m)) (symbolVal (Proxy @p)) False 

instance GHasTypeInfo (a :+: b) where
  gGetTypeInfo _ = error "not defined"

instance GHasTypeInfo (a :*: b) where
  gGetTypeInfo _ = error "not defined"

instance GHasTypeInfo (C n a) where
  gGetTypeInfo _ = error "not defined"

instance GHasTypeInfo (V a) where
  gGetTypeInfo _ = error "not defined"

instance GHasTypeInfo (Rec a) where
  gGetTypeInfo _ = error "not defined"

instance GHasTypeInfo U where
  gGetTypeInfo _ = error "not defined"
