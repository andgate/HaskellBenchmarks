module BinarySearch (search) where

import Data.Vector.Unboxed as U
import Data.Maybe

search :: U.Vector Int -> Int -> Maybe Int
search v x = search' v x 0 (U.length v)
    where
        search' v x lo hi
            | lo > hi   = Nothing
            | x > curr  = search' v x (mid + 1) hi
            | x < curr  = search' v x lo (mid - 1)
            | otherwise = Just mid
            where
                mid = (hi + lo) `div` 2
                curr = (U.unsafeIndex v mid)