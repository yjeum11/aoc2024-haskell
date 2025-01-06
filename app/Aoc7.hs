{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc7 where

aoc7 :: IO ()
aoc7 = do
    contents <- readFile "input"
    -- contents <- readFile "example"
    print $ sum $ map (part1 . parse) $ lines contents
    print $ sum $ map (part2 . parse) $ lines contents

parse :: String -> (Int, [Int])
parse s = (read n, map read nums)
    where (n, numstr) = break (==':') s
          nums = tail $ words numstr

part1 :: (Int, [Int]) -> Int
part1 (target, n:nums) = part1' target nums n
    where part1' t (x:xs) curr = if part1' t xs (curr+x) > 0 || part1' t xs (curr*x) > 0 then t else 0
          part1' t [] curr = if t == curr then t else 0
part1 (_, _) = 0

(|||) :: Int -> Int -> Int
a ||| b = read $ show a ++ show b

part2 :: (Int, [Int]) -> Int
part2 (target, n:nums) = part2' target nums n
    where part2' t (x:xs) curr = if part2' t xs (curr+x) > 0 || part2' t xs (curr*x) > 0 || part2' t xs (curr|||x) > 0 then t else 0
          part2' t [] curr = if t == curr then t else 0
part2 (_, _) = 0

