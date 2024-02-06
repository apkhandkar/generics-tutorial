{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Generics
import Data.MyEq
import Data.MyShow

data Signal = Red | Amber | Green
  deriving Show

instance Generic Signal where

  type Rep Signal = (C "Red" U) :+: (C "Amber" U) :+: (C "Green" U)

  from :: Signal -> Rep Signal
  from Red = L (C U)
  from Amber = R (L (C U))
  from Green = R (R (C U))

  to :: Rep Signal -> Signal
  to (L (C U)) = Red
  to (R (L (C U))) = Amber
  to (R (R (C U))) = Green

instance MyEq Signal

instance MyShow Signal

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where

  type Rep (Tree a) = (C "Leaf" (V a)) :+: (C "Node" ((Rec (Tree a)) :*: (Rec (Tree a))))

  from :: Tree a -> Rep (Tree a)
  from (Leaf a) = L (C (V a))
  from (Node l r) = R (C ((Rec l) :*: (Rec r)))

  to :: Rep (Tree a) -> Tree a
  to (L (C (V a))) = Leaf a
  to (R (C ((Rec l) :*: (Rec r)))) = Node l r

instance MyEq a => MyEq (Tree a)

instance MyShow a => MyShow (Tree a)

main :: IO ()
main = do

  let t1 = Node (Node (Leaf Red) (Leaf Red)) (Node (Leaf Green) (Node (Leaf Amber) (Leaf Red)))
      t2 = Node (Node (Leaf Green) (Node (Leaf Amber) (Leaf Amber))) (Leaf Red)
      t3 = Node (Node (Leaf Red) (Leaf Red)) (Node (Leaf Green) (Node (Leaf Amber) (Leaf Red)))

  putStrLn $ show (t1 `myEq` t2)
  putStrLn $ show (t1 `myEq` t3)

  putStrLn $ myShow t1
  putStrLn $ myShow t2
