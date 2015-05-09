module QuadraticCount (count) where

import CountFrequency

import qualified Data.Vector as Vec
import Data.Vector (Vector, (!))

count :: Vector Integer -> Vector (Integer, Integer)
count vec = Vec.map (countFreq vec) vec