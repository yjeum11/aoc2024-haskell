{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc6 where

import Data.List
import Data.List.Split
import Data.Char
import Data.Map
import Data.Set

-- aoc6 :: IO ()
-- aoc6 = do
--     contents <- readFile "input"
--     -- contents <- readFile "example"
--     -- print $ part1 contents
--
aoc6 :: IO ()
aoc6 = return ()

data Direction = N | S | E | W

turn :: Direction -> Direction
turn N = E
turn S = W
turn E = S
turn W = N

offset :: Direction -> (Int, Int)
offset N = (-1, 0)
offset S = (1, 0)
offset E = (0, 1)
offset W = (0, -1)

data DirPoint = DirPoint {pos :: (Int, Int), dir :: Direction}
newtype Guard = Guard DirPoint
newtype Point = Point DirPoint

