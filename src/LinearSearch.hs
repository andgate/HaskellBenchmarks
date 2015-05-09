module LinearSearch (search) where

import Data.Vector as Vec
import Data.Maybe

search :: Vector Integer -> Integer -> Maybe Integer
search vec x = search' vec x $ (Vec.length vec) - 1
    where
        search' vec x i
            | i < 0      = Nothing
            | x == vec!i = Just x
            | otherwise  = search' vec x (i - 1)