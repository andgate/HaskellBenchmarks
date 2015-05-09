module BinarySearch (search) where

import qualified Data.Vector as Vec
import Data.Vector (Vector, (!))
import Data.Maybe

search :: Vector Integer -> Integer -> Maybe Integer
search vec x = search' vec x 0 (Vec.length vec)
    where
        search' vec x lo hi
            | lo >= hi   = Nothing
            | x > curr  = search' vec x (mid + 1) hi
            | x < curr  = search' vec x lo (mid - 1)
            | otherwise = Just (toInteger mid)
            where
                mid = lo + (hi - lo) `div` 2
                curr = vec!mid