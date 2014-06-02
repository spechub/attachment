module Product where

t1,t2,t3,t4 :: (Int,Int)
t1 = undefined
t2 = (undefined,undefined)
t3 = (undefined,0)
t4 = (0,undefined)
t5 = (0,0)


different_t1_t2 = seq t1 1
different_t1_t2a = seq t2 1

different_t2_t3 = snd t2
different_t2_t3a = snd t3

different_t3_t4 = snd t3
different_t3_t4a = snd t4

different_t4_t5 = snd t4
different_t4_t5a = snd t5
