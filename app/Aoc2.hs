{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Aoc2 where

import Data.List

aoc2 :: IO ()
aoc2 = do
    contents <- readFile "input"
    print $ solution $ parse contents

parse :: String -> [[Int]]
parse s = map (map read . words) $ lines s

solution :: [[Int]] -> Int
solution xss = length $ filter isSafeBruteForce xss

edgecases xss = (a \\ b) `union` (b \\ a)
    where a = filter isSafe2 xss
          b = filter isSafeBruteForce xss

allRemove :: [a] -> [[a]]
allRemove [] = [[]]
allRemove (x:xs) = xs : map (x :) (allRemove xs)

data Direction = Up | Down

isSafeBruteForce :: [Int] -> Bool
isSafeBruteForce xss = any isSafe $ allRemove xss

isSafe :: [Int] -> Bool
isSafe l@(x:y:_)
    | x < y = isSafe' l Up
    | x > y = isSafe' l Down
    | otherwise = False
isSafe _ = True

isSafe' :: [Int] -> Direction -> Bool
isSafe' (a:b:rest) Up = if 1 <= b - a && b - a <= 3 then isSafe' (b:rest) Up else False
isSafe' (a:b:rest) Down = if 1 <= a - b && a - b <= 3 then isSafe' (b:rest) Down else False
isSafe' _ _ = True

isSafe2 :: [Int] -> Bool
isSafe2 l = gawktuah l Up || gawktuah l Down || isSafe (tail l)
    where gawktuah (a:b:rest) Up =
            if 1 <= b - a && b - a <= 3 then
                gawktuah (b:rest) Up
            else isSafe' (a:rest) Up
          gawktuah (a:b:rest) Down =
            if 1 <= a - b && a - b <= 3 then
                gawktuah (b:rest) Down
            else isSafe' (a:rest) Down
          gawktuah _ _ = True
