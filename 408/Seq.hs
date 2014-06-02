module Seq where

bottom :: a->b
bottom = undefined
lambda_x_bottom :: a->b 
lambda_x_bottom = \ x -> undefined

c1 = seq bottom 1
c2 = seq lambda_x_bottom 1
