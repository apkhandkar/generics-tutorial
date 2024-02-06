{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Generics
import Data.MyEq
import Data.MyShow

data Signal = Red | Amber | Green
  deriving Show

instance Generic Signal where

  type Rep Signal = (M "Red" U) :+: (M "Amber" U) :+: (M "Green" U)

  from :: Signal -> Rep Signal
  from Red = L (M U)
  from Amber = R (L (M U))
  from Green = R (R (M U))

  to :: Rep Signal -> Signal
  to (L (M U)) = Red
  to (R (L (M U))) = Amber
  to (R (R (M U))) = Green

instance MyEq Signal

instance MyShow Signal

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where

  type Rep (Tree a) = (M "Leaf" (V a)) :+: (M "Node" ((Rec (Tree a)) :*: (Rec (Tree a))))

  from :: Tree a -> Rep (Tree a)
  from (Leaf a) = L (M (V a))
  from (Node l r) = R (M ((Rec l) :*: (Rec r)))

  to :: Rep (Tree a) -> Tree a
  to (L (M (V a))) = Leaf a
  to (R (M ((Rec l) :*: (Rec r)))) = Node l r

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
