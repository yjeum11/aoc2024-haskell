module Aoc4 where

import Data.List

aoc4 :: IO ()
aoc4 = do
    contents <- readFile "input"
    -- contents <- readFile "example"
    print $ part1 $ lines contents
    print $ part2 $ parse2 $ lines contents

part1 :: [String] -> Int
part1 l = forward l + forward (reverse $ map reverse l)

forward :: [String] -> Int
forward l = countRows l + countCols l + countRows (diagonals l) + countRows (antidiags l)

countRows :: [String] -> Int
countRows = sum . map go
    where go (a:b:c:d:rest)
            | [a,b,c,d] == "XMAS" = 1 + go (b:c:d:rest)
            | otherwise = go (b:c:d:rest)
          go _ = 0

countCols :: [String] -> Int
countCols = countRows . transpose

diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

antidiags :: [[a]] -> [[a]]
antidiags = diagonals . map reverse

-- -- grid to list of X's [center, (left diag, right diag)]
parse2 :: [[a]] -> [(a, [a], [a])]
parse2 g =
    let
        hpad l = Nothing:map Just l++[Nothing]
        vpad l = Nothing <$ hpad l
        padded = concat [[vpad $ head g], map hpad g, [vpad $ head g]]
        annotateRowTriple ((Just tl:ts@(_:Just tr:_)):
                           (_:cs@(Just c:_:_)):
                           (Just bl:bs@(_:Just br:_)):
                           rest) =
            (c, [tl, c, br], [tr, c, bl]) : annotateRowTriple (ts:cs:bs:rest)
        annotateRowTriple ((_:top):(_:mid):(_:bot):rest) = annotateRowTriple (top:mid:bot:rest)
        annotateRowTriple _ = []
        annotate r@(_:mid:bot:rest) = annotateRowTriple r ++ annotate (mid:bot:rest)
        annotate _ = []
    in
        annotate padded

part2 :: [(Char, String, String)] -> Int
part2 (('A', l, r):rest) =
    if (l == "MAS" || l == "SAM") && (r == "MAS" || r == "SAM") then
        1 + part2 rest
    else
        part2 rest
part2 (_:rest) = part2 rest
part2 [] = 0
