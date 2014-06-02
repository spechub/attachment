module Types where
  data D1 = D1 Int
  data D2 = D2 !Int
  type S = Int
  newtype N = N Int
  d1 (D1 i) = 42
  d2 (D2 i) = 42
  s i = 42
  n (N i) = 42
