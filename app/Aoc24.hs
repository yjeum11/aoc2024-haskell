module Aoc24 where

aoc24 :: IO ()
aoc24 = do
    contents <- readFile "input"
    putStrLn contents

-- vertices are gates, edges are wires (alias for String)
-- name, in1, in2, out
data Gate = And Int | Or Int | Xor Int | Init Int Bool

-- Gate, fanout list.
type Graph a = [(a, [a])]

evaluate :: Graph Gate -> [Bool]
evaluate = 
