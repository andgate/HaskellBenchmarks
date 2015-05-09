module LogarithmicCount (count) where

import CountFrequency

import qualified Data.Vector as Vec
import Data.Vector (Vector, (!))

import Control.Monad.ST
import Control.Monad

import Data.Vector.Algorithms.Merge as Merge

count :: Vector Integer -> Vector (Integer, Integer)
count vec = Vec.imap (countRepeats vec_sorted) vec_sorted
    where
        vec_sorted = runST $ do
            mvec <- Vec.unsafeThaw vec
            Merge.sort mvec
            Vec.unsafeFreeze mvec