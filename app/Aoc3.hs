module Aoc3 where

import Text.Regex.TDFA

aoc3 :: IO ()
aoc3 = do
    contents <- readFile "input"
    print $ solution $ parse contents

instrRegex :: String
instrRegex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)"

data Instr = Mul Int Int | Do | Dont

parse :: String -> [Instr]
parse s = let (_, matched, remaining, matches) = s =~ instrRegex :: (String, String, String, [String]) in
    case matched of
        "do()" -> Do : parse remaining
        "don't()" -> Dont : parse remaining
        _ -> case matches of
                [a, b] -> Mul (read a) (read b) : parse remaining
                _ -> []

solution :: [Instr] -> Int
solution l = go l True
    where go [] _ = 0
          go (t:xs) d = case t of
            Do -> go xs True
            Dont -> go xs False
            Mul a b -> if d then a*b + go xs d else go xs d

