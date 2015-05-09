module CountFrequency (countFreq, countRepeats) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))

countFreq :: Vector Integer -> Integer -> (Integer, Integer)
countFreq v e = (e, countFreq' v e $ (V.length v) - 1)
    where
        countFreq' v e i
            | i < 0     = 0
            | e == v!i  = 1 + countFreq' v e (i - 1)
            | otherwise = countFreq' v e (i - 1)

countRepeats :: Vector Integer -> Int -> Integer -> (Integer, Integer)
countRepeats v i e = (e, countRepeats' v (i+1) e 1)

countRepeats' :: Vector Integer -> Int -> Integer -> Integer -> Integer
countRepeats' v i e n
    | i >= (V.length v) = n
    | e == (v!i)        = countRepeats' v (i+1) e (n+1)
    | otherwise         = n