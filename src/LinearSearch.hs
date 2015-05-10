module LinearSearch (search) where

import Data.Vector.Unboxed as U
import Data.Maybe

search :: U.Vector Int -> Int -> Maybe Int
search v x = search' v x $ (U.length v) - 1
    where
        search' v x i
            | i < 0      = Nothing
            | x == (U.unsafeIndex v i) = Just x
            | otherwise  = search' v x (i - 1)