{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Generics
import Data.MyEq

data Signal = Red | Amber | Green
  deriving Show

instance Generic Signal where

  type Rep Signal = U :+: U :+: U

  from :: Signal -> Rep Signal
  from Red = L U 
  from Amber = R (L U)
  from Green = R (R U)

  to :: Rep Signal -> Signal
  to (L U) = Red
  to (R (L U)) = Amber
  to (R (R U)) = Green

instance MyEq Signal

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Generic (Tree a) where

  type Rep (Tree a) = (V a) :+: (Rec (Tree a)) :*: (Rec (Tree a))

  from :: Tree a -> Rep (Tree a)
  from (Leaf a) = L (V a)
  from (Node l r) = R ((Rec l) :*: (Rec r))

  to :: Rep (Tree a) -> Tree a
  to (L (V a)) = Leaf a
  to (R ((Rec l) :*: (Rec r))) = Node l r

instance MyEq a => MyEq (Tree a)

main :: IO ()
main = do

  let t1 = Node (Node (Leaf Red) (Leaf Red)) (Node (Leaf Green) (Node (Leaf Amber) (Leaf Red)))
      t2 = Node (Node (Leaf Green) (Node (Leaf Amber) (Leaf Amber))) (Leaf Red)
      t3 = Node (Node (Leaf Red) (Leaf Red)) (Node (Leaf Green) (Node (Leaf Amber) (Leaf Red)))

  putStrLn $ show (t1 `myEq` t2)
  putStrLn $ show (t1 `myEq` t3)
