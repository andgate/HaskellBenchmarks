module QuadraticCount (count) where

import CountFrequency

import Data.Vector.Unboxed as U

count :: U.Vector Int -> U.Vector (Int, Int)
count vec = U.map (countFreq vec) vec