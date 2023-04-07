module PickRandom (pickRandom) where
import Data.Time.Clock.System
import Data.Bits

-- a home-made random selector
-- when System.Random is not installed

pickRandom :: [a] -> IO a
pickRandom [] = undefined
pickRandom [x] = return x
pickRandom xs = do
   t <- getSystemTime
   let n = systemNanoseconds t
   let nn = fromIntegral(shiftR (n * 123456789) 16) -- some kind of multiplicative hashing
   let m = length xs
   return (xs!! (mod nn m))
   
    
