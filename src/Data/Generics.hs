{-# LANGUAGE TypeFamilies #-}
module Data.Generics where

import Data.Kind

-- | Types that can be represented.
class Generic a where

  -- | Representation of the type
  type Rep a :: Type
 
  -- | Convert from the type to its representation
  from :: a -> Rep a

  -- | Convert from the representation to the type
  to :: Rep a -> a


-- ** Types that representations are built out of 

-- | Sum types
data a :+: b = L a | R b
  deriving Show

infixr 5 :+:

-- | Product types
data a :*: b = a :*: b
  deriving Show

infixr 6 :*:

-- | Values wrapped within constructors
newtype V a = V { unV :: a }
  deriving Show

-- | Recursive instances of types
newtype Rec a = Rec { unRec :: a }
  deriving Show

-- | Constructors that take no arguments
data U = U 
  deriving Show
