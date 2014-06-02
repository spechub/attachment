
module Main where

import Data.Typeable
import Data.Dynamic

coerce :: (Typeable a, Typeable b) => a -> Maybe b
coerce = fromDynamic . toDyn

v1 :: Int
v1 = 50

v2 :: String
v2 = "50"

newtype A1 = MkA1 Int
newtype A2 = MkA2 Float

a1TyCon, a2TyCon :: TyCon
a1TyCon = mkTyCon "a"
a2TyCon = mkTyCon "a"

instance Show A1 where
    show (MkA1 x) = show x

instance Show A2 where
    show (MkA2 x) = show x

instance Typeable A1 where
    typeOf _ = mkTyConApp (a1TyCon) []

instance Typeable A2 where
    typeOf _ = mkTyConApp (a2TyCon) []

main :: IO ()
main = do
  putStrLn $ show (cast v1 :: Maybe String)
  putStrLn $ maybe "no cast" (\x -> show (x == v1)) $ (cast v2 :: Maybe Int)
  putStrLn $ show (cast (MkA1 v1) :: Maybe A2)
  putStrLn $ show (coerce (MkA1 70) :: Maybe A2)
  typeRepKey (typeOf $ MkA1 50) >>= putStrLn . show 
  typeRepKey (typeOf $ MkA2 2.0) >>= putStrLn . show 