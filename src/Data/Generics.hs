module Data.Generics where

import Data.Kind
import GHC.TypeLits

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

data Fixity = Prefix | Infix
  deriving Show

-- | Constructor metadata
data C (n :: Symbol) (f :: Fixity) a = C { unC :: a }
  deriving Show

-- | Type metadata
data M (n :: Symbol) (m :: Symbol) (p :: Symbol) (nt :: Bool) (a :: Type) = M { unM :: a }
  deriving Show

-- | Record field selectors
data S (n :: Symbol) a = S { unS :: a }
  deriving Show

-- ** Instances for commonly used types

instance Generic Bool where

  type Rep Bool = M "Bool" "Prelude" "base" False ((C "False" Prefix U) :+: (C "True" Prefix U))

  from :: Bool -> Rep Bool
  from False = M (L (C U))
  from True = M (R (C U))

  to :: Rep Bool -> Bool
  to (M (L (C U))) = False
  to (M (R (C U))) = True

instance Generic [a] where

  type Rep [a] = M "[]" "GHC.List" "base" False ((C "[]" Prefix U) :+: (C ":" Infix ((V a) :*: (Rec [a]))))

  from :: [a] -> Rep [a]
  from [] = M (L (C U))
  from (a:as) = M (R (C ((V a) :*: (Rec as))))

  to :: Rep [a] -> [a]
  to (M (L (C U))) = []
  to (M (R (C ((V a) :*: (Rec as))))) = a:as








