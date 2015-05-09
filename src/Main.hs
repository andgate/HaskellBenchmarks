-- | Main entry point to the application.
module Main where

import LinearSearch
import BinarySearch
import QuadraticCount
import LogarithmicCount

import Control.DeepSeq

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import System.Random
import System.CPUTime

type IntegerArray = Vector Integer
type IntegerArray2 = Vector (Integer, Integer)
type ArraySearcher = IntegerArray -> Integer -> Maybe Integer
type ArrayCounter = IntegerArray -> IntegerArray2
type SearchQuery = (Integer, IntegerArray)
type BenchmarkResult = (Integer, Integer)

-- | The main entry point.
main :: IO ()
main = do
    let ns = [1000, 2000 .. 6000]
    let vecs = sortedVecs ns

    results <- benchmarkSearches (LinearSearch.search) $ force (zip ns vecs)
    putStrLn $ showBenchmarks "Linear Search" (zip ns results)
    putStrLn ""

    results <- benchmarkSearches (BinarySearch.search) $ force (zip ns vecs)
    putStrLn $ showBenchmarks "Binary Search" (zip ns results)
    putStrLn ""

    vecs <- randomVecs (1, 100) ns

    -- QUADRATIC COUNT IS REALLY SLOW
    -- DISABLED UNTIL NEEDED
    --results <- benchmarkCounts (QuadraticCount.count) $ force vecs
    --putStrLn $ showBenchmarks "Quadratic Count" (zip ns results)
    --putStrLn ""

    results <- benchmarkCounts (LogarithmicCount.count) $ force vecs
    putStrLn $ showBenchmarks "Logarithmic Count" (zip ns results)
    putStrLn ""

showBenchmarks :: String -> [BenchmarkResult] -> String
showBenchmarks name ts = name ++ "\n" ++ (foldr (\(ns, time) acc -> (show ns) ++ ": " ++ (show time) ++ "\n" ++ acc) "" ts)

benchmarkSearches :: ArraySearcher -> [SearchQuery] -> IO [Integer]
benchmarkSearches search q = (benchmarkSearch search) `mapM` q

benchmarkSearch :: ArraySearcher -> SearchQuery -> IO Integer
benchmarkSearch search (x, vec) = do
    start <- getTime
    let r = search vec x
    end <- r `deepseq` getTime
    return (end - start)

benchmarkCounts :: ArrayCounter -> [IntegerArray] -> IO [Integer]
benchmarkCounts count vec = (benchmarkCount count) `mapM` vec

benchmarkCount :: ArrayCounter -> IntegerArray -> IO Integer
benchmarkCount count vec = do
    start <- getTime
    let r = count vec
    end <- r `deepseq` getTime
    return (end - start)

sortedVecs :: [Integer] -> [IntegerArray]
sortedVecs ns = map sortedVec ns

sortedVec :: Integer -> IntegerArray
sortedVec n = Vec.generate (fromIntegral n) $ \x -> (fromIntegral x)


randomVecs :: (Integer, Integer) -> [Integer] -> IO [IntegerArray]
randomVecs range ns = (randomVec range) `mapM` ns

randomVec :: (Integer, Integer) -> Integer -> IO IntegerArray
randomVec range@(hi, lo) n = do
    g <- getStdGen
    let rs =  randomRs range g
    let vec = Vec.fromListN (fromIntegral n) rs
    return vec

getTime :: IO Integer
getTime = do
    pico <- getCPUTime
    return (pico `div` 1000000000)