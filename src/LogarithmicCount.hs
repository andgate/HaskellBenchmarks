module LogarithmicCount (count) where

import CountFrequency

import qualified Data.Vector.Unboxed as U

import Control.Monad.ST
import Control.Monad

import Data.Vector.Algorithms.Intro as Intro

count :: U.Vector Int -> U.Vector (Int, Int)
count vec = U.imap (countRepeats vec_sorted) vec_sorted
    where
        vec_sorted = runST $ do
            mvec <- U.unsafeThaw vec
            Intro.sort mvec
            U.unsafeFreeze mvec

countRepeats :: U.Vector Int -> Int -> Int -> (Int, Int)
countRepeats v i e = (e, U.length . U.takeWhile (==e) . U.drop i $ v)