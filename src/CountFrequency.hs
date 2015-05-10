{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module CountFrequency (countFreq, countRepeats) where

import qualified Data.Vector.Unboxed as U

import Control.DeepSeq

import GHC.Exts

countFreq :: U.Vector Int -> Int -> (Int, Int)
countFreq v e = (e, U.length . U.filter (==e) $ v)
{-# INLINE countFreq #-}

countRepeats :: U.Vector Int -> Int -> Int -> (Int, Int)
countRepeats v i e = (e, U.length . U.takeWhile (==e) . U.drop i $ v)
{-# INLINE countRepeats #-}