{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Aoc1 where

import Data.List (transpose, sort)

aoc1 :: IO ()
aoc1 = do
    contents <- readFile "input"
    print $ solution2 $ parse contents

parse :: String -> [[Int]]
parse s = transpose $ map (map read . words) (lines s)

solution :: [[Int]] -> Int
solution (xs:ys:_) = sum $ zipWith dist (sort xs) (sort ys)
    where dist x y = abs $ x - y
solution _ = error "malformed input"

solution2 :: [[Int]] -> Int
solution2 (xs:ys:_) = go xs ys 0
    where count _ [] = 0
          count x (y:ys) 
            | x == y = count x ys + 1 
            | otherwise = count x ys
          go (a:as) bs curr = go as bs (curr+(a*count a bs))
          go [] _ curr = curr
solution2 _ = error "malformed input"
