module TypeClass2 where


data T1 = T1 Int
data T2 = T2 Int
data T3 = T3 Int

class C a where
  f,g :: a -> Bool
  f x = not (g x)
  g x = not (f x)
  h :: a -> Bool
  h x = not (h x)
instance C T1 where -- no recursion
  f x = True

instance C T2  -- should lead to mutual recursion

instance C T3 where  -- should lead to mutual recursion
  f x = not (not (g x))
  g x = not (not (f x))
