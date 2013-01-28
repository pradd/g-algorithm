module Evaluator (evaluate) where

import Data.List (sortBy)
import Data.Function (on)

import Dna
import Mutator(mutateDna)
import qualified Config

evaluate :: Int -> [DNA] -> [DNA]
evaluate 0 xdna = xdna
evaluate i xdna = evaluate (i-1) (filterByScore $ concat $ map f xdna)
    where   f :: DNA -> [(DNA, Score)]
            f dna = map (calcScore . mutateDna) $ breed dna

breed :: DNA -> [DNA]
breed = replicate Config.fertility

calcScore :: DNA -> (DNA, Score)
calcScore d = (d, Config.score d)

filterByScore :: [(DNA, Score)] -> [DNA]
filterByScore ps = selectNewGeneration $ map fst $ sortBy (compare `on` snd) ps

selectNewGeneration :: [DNA] -> [DNA]
selectNewGeneration xs | length xs <= Config.maxPopulation = xs
                       | otherwise = take Config.maxPopulation xs
