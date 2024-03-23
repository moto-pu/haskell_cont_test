{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Cont where

type Cont r a = (a -> r) -> r

fact_cps :: Integer -> Cont r Integer
fact_cps 0 cont = cont 1
fact_cps n cont = fact_cps (n - 1) (\x -> cont $ n * x)

fibo_cps :: Integer -> Cont r Integer
fibo_cps 0 cont = cont 1
fibo_cps 1 cont = cont 1
fibo_cps n cont =
  fibo_cps (n - 1) (\x -> fibo_cps (n - 2) (\y -> cont (x + y)))

flatten_cps :: [[a]] -> Cont [b] [a]
flatten_cps [] cont = cont []
flatten_cps ([]:_) cont = []
flatten_cps (x:xs) cont = flatten_cps xs (\y -> cont (x ++ y))


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

a = Node (Leaf 1) (Node (Leaf 2) (Node (Leaf 3) (Leaf 4)))
b = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
c = Node (Leaf 1) (Node (Leaf 2) (Node (Leaf 4) (Leaf 3)))

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node l r) = toList l ++ toList r

sameFringe :: Eq a => Tree a -> Tree a -> Bool
sameFringe a b = toList a == toList b

data Cont' a = Null | Cont' a (() -> Cont' a)

traverse_tree :: Tree a -> Cont (Cont' a) ()
traverse_tree (Leaf x) cont = Cont' x cont
traverse_tree (Node l r) cont = traverse_tree l (\() -> traverse_tree r cont)

sameFringe' :: Eq a => Tree a -> Tree a -> Bool
sameFringe' a b =
  iter (traverse_tree a (\() -> Null))
       (traverse_tree b (\() -> Null))
  where iter Null Null = True
        iter (Cont' x na) (Cont' y nb) =
          if x /= y then False
          else iter (na()) (nb())
        iter _ _ = False