module Evaluator (evaluate) where

import Data.List (sortBy)
import Data.Function (on)

import Dna
import Mutator(mutateDna)

evaluate :: Int -> [DNA] -> [DNA]
evaluate 0 xdna = xdna
evaluate i xdna = evaluate (i-1) (filterByScore $ concat $ map f xdna)
    where   f :: DNA -> [(DNA, Score)]
            f dna = map (score . mutateDna) $ breed dna

breed :: DNA -> [DNA]
breed = replicate 3

score :: DNA -> (DNA, Score)
score d@(DNA x) = (d, abs x)

filterByScore :: [(DNA, Score)] -> [DNA]
filterByScore ps = selectNewGeneration $ map fst $ sortBy (compare `on` snd) ps

selectNewGeneration :: [DNA] -> [DNA]
selectNewGeneration xs | length xs <= 100 = xs
                       | otherwise = take 100 xs
