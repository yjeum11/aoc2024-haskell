{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc5 where

import Data.List
import Data.List.Split
import Data.Char

aoc5 :: IO ()
aoc5 = do
    contents <- readFile "input"
    -- contents <- readFile "example"
    print $ part1 contents

part1 :: String -> Int
part1 s = sum $ map middle $ filter (validate rs) ps
    where rs = rules s
          ps = pages s
          middle x = x !! (length x `div` 2)

rules :: String -> [(Int, Int)]
rules s = map process rulelist
    where (rulestr, _) = break null $ lines s
          rulelist = map (span isDigit) rulestr
          process l = case l of
            (f, _:sn) -> (read f, read sn)
            _ -> error "malformed"

pages :: String -> [[Int]]
pages s = map read . splitOn "," <$> pagestr
    where (_, _:pagestr) = break null $ lines s

validate :: [(Int, Int)] -> [Int] -> Bool
validate ((f,s):rest) p = 
    case (elemIndex f p, elemIndex s p) of
        (Just i, Just j) -> i < j
        _ -> True
    && validate rest p
validate [] _ = True
