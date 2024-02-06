{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Generics
import Data.MyEq
import Data.MyShow
import Data.TypeInfo

data Signal = Red | Amber | Green
  deriving Show

instance Generic Signal where

  type Rep Signal =
    M "Signal" "Main" "generics" False ((C "Red" Prefix U) :+: (C "Amber" Prefix U) :+: (C "Green" Prefix U))

  from :: Signal -> Rep Signal
  from Red = M (L (C U))
  from Amber = M (R (L (C U)))
  from Green = M (R (R (C U)))

  to :: Rep Signal -> Signal
  to (M (L (C U))) = Red
  to (M (R (L (C U)))) = Amber
  to (M (R (R (C U)))) = Green

instance MyEq Signal

instance MyShow Signal

instance HasTypeInfo Signal

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where

  type Rep (Tree a) =
    M "Tree" "Main" "generics" False
      ((C "Leaf" Prefix (V a))
        :+: (C "Node" Prefix ((Rec (Tree a)) :*: (Rec (Tree a)))))

  from :: Tree a -> Rep (Tree a)
  from (Leaf a) = M (L (C (V a)))
  from (Node l r) = M (R (C ((Rec l) :*: (Rec r))))

  to :: Rep (Tree a) -> Tree a
  to (M (L (C (V a)))) = Leaf a
  to (M (R (C ((Rec l) :*: (Rec r))))) = Node l r

instance MyEq a => MyEq (Tree a)

instance MyShow a => MyShow (Tree a)

instance HasTypeInfo (Tree a)

data SomeInfo = SomeInfo
  { signal :: Signal
  , boolTree :: Tree Bool
  , isUseful :: Bool
  , otherSignals :: [Signal]
  }

instance Generic SomeInfo where

  type Rep SomeInfo =
    M "SomeInfo" "Main" "generics" False
      (C "SomeInfo" Prefix
        ((S "signal" (V Signal))
          :*: (S "boolTree" (V (Tree Bool)))
          :*: (S "isUseful" (V Bool))
          :*: (S "otherSignals" (V [Signal]))))

  from :: SomeInfo -> Rep SomeInfo
  from (SomeInfo{..}) = M (C ((S (V signal)) :*: (S (V boolTree)) :*: (S (V isUseful)) :*: (S (V otherSignals))))

  to :: Rep SomeInfo -> SomeInfo
  to (M (C ((S (V signal)) :*: (S (V boolTree)) :*: (S (V isUseful)) :*: (S (V otherSignals))))) = SomeInfo{..}

instance MyEq SomeInfo

instance MyShow SomeInfo

instance HasTypeInfo SomeInfo

main :: IO ()
main = do

  let t1 = Node (Node (Leaf Red) (Leaf Red)) (Node (Leaf Green) (Node (Leaf Amber) (Leaf Red)))
      t2 = Node (Node (Leaf Green) (Node (Leaf Amber) (Leaf Amber))) (Leaf Red)
      t3 = Node (Node (Leaf Red) (Leaf Red)) (Node (Leaf Green) (Node (Leaf Amber) (Leaf Red)))

  putStrLn $ myShow (t1 `myEq` t2)
  putStrLn $ myShow (t1 `myEq` t3)

  putStrLn $ myShow t1
  putStrLn $ myShow t2

  putStrLn $ show $ getTypeInfo t1

  let someInfo1 =
        SomeInfo
          Red
          (Node (Node (Node (Leaf True) (Leaf False)) (Leaf True)) (Node (Leaf False) (Leaf True)))
          False
          [Red, Amber, Green, Green]
      someInfo2 =
        SomeInfo
          Green
          (Node (Leaf False) (Leaf True))
          True
          [Red, Amber, Green]
      someInfo3 =
        SomeInfo
          Red
          (Node (Node (Node (Leaf True) (Leaf False)) (Leaf True)) (Node (Leaf False) (Leaf True)))
          False
          [Red, Amber, Green, Green]
  
  putStrLn $ myShow someInfo2
  putStrLn $ myShow (someInfo1 `myEq` someInfo2)
  putStrLn $ myShow (someInfo1 `myEq` someInfo3)
