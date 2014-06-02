module TypeClass where

data T1 = T1
data T2 = T2
data T3 = T3

class C a where
  c :: a -> Int
  c x = 1
instance C T1 where
  c x = 2
instance C T2 where
  c x = 3
instance C T3

d :: Int -> Int
d x = c T1 + c T2 + c T3