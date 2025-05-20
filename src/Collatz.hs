module Collatz where

import Data.List (maximumBy)

collatz :: Int -> Int
collatz n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1

collatzSeries :: Int -> [Int]
collatzSeries 1 = [1]
collatzSeries n = n : collatzSeries (collatz n)

collatzSeriesLen :: [Int] -> Int
collatzSeriesLen = length

longest :: Int
longest = fst $ maximumBy compareSnd [(x, collatzSeriesLen (collatzSeries x)) | x <- [1..1000000]]
  where
    compareSnd :: (Int, Int) -> (Int, Int) -> Ordering
    compareSnd (_, len1) (_, len2) = compare len1 len2