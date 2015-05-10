-- | Main entry point to the application.
module Main where

import LinearSearch
import BinarySearch
import QuadraticCount
import LogarithmicCount

import Control.DeepSeq

import qualified Data.Vector.Unboxed as U

import System.Random
import System.CPUTime

type IntArray = U.Vector Int
type IntArray2 = U.Vector (Int, Int)
type ArraySearcher = IntArray -> Int -> Maybe Int
type ArrayCounter = IntArray -> IntArray2
type SearchQuery = (Int, IntArray)
type BenchmarkResult = (Int, Integer)

-- | The main entry point.
main :: IO ()
main = do
    let ns = map (*1000000) [1, 2 .. 6] :: [Int]
    let v = force $ zip ns $ sortedVecs ns

    results <- benchmarkSearches (LinearSearch.search) v
    putStrLn $ showBenchmarks "Linear Search" (zip ns results)
    putStrLn ""

    results <- benchmarkSearches (BinarySearch.search) v
    putStrLn $ showBenchmarks "Binary Search" (zip ns results)
    putStrLn ""

    let ns = map (*10000) [1, 2 .. 6] :: [Int]
    v <- randomVecs (1, 100) ns

    -- QUADRATIC COUNT IS REALLY SLOW
    -- DISABLED UNTIL NEEDED
    results <- benchmarkCounts (QuadraticCount.count) $ force v
    putStrLn $ showBenchmarks "Quadratic Count" (zip ns results)
    putStrLn ""

    results <- benchmarkCounts (LogarithmicCount.count) $ force v
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

benchmarkCounts :: ArrayCounter -> [IntArray] -> IO [Integer]
benchmarkCounts count vec = (benchmarkCount count) `mapM` vec

benchmarkCount :: ArrayCounter -> IntArray -> IO Integer
benchmarkCount count vec = do
    start <- getTime
    let r = count vec
    end <- r `deepseq` getTime
    return (end - start)

sortedVecs :: [Int] -> [IntArray]
sortedVecs ns = map sortedVec ns

sortedVec :: Int -> IntArray
sortedVec n = U.enumFromN 1 n


randomVecs :: (Int, Int) -> [Int] -> IO [IntArray]
randomVecs range ns = (randomVec range) `mapM` ns

randomVec :: (Int, Int) -> Int -> IO IntArray
randomVec range@(hi, lo) n = do
    g <- getStdGen
    let rs =  randomRs range g
    let vec = U.fromListN n rs
    return vec

picoToNano :: Integer -> Integer
picoToNano x = x `div` 1000

picoToMilli :: Integer -> Integer
picoToMilli x = x `div` 1000000000

getTime :: IO Integer
getTime = do
    pico <- getCPUTime
    return $ picoToMilli pico