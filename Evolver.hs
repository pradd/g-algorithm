module Evolver (evolve) where

import Data.List (sortBy)
import Data.Function (on)
import System.Random (StdGen, split)

import Dna
import Mutator (mutateDna)
import qualified Config

evolve :: StdGen -> [DNA] -> [DNA]
evolve rnd xdna = evaluate rnd Config.iterations xdna

evaluate :: StdGen -> Int -> [DNA] -> [DNA]
evaluate _   0 xdna = xdna
evaluate rnd i xdna = evaluate (head rs) (i-1) (filterByScore $ concat $ map f xdna)
    where   f :: DNA -> [(DNA, Score)]
            f dna = map (calcScore . mutateDna) $ zip (tail rs) $ breed dna
            rs = randomGens rnd

randomGens ::  StdGen -> [StdGen]
randomGens rnd = iterate (fst . split) rnd

breed :: DNA -> [DNA]
breed = replicate Config.fertility

calcScore :: DNA -> (DNA, Score)
calcScore d = (d, Config.score d)

filterByScore :: [(DNA, Score)] -> [DNA]
filterByScore ps = selectNewGeneration $ map fst $ sortBy (compare `on` snd) ps

selectNewGeneration :: [DNA] -> [DNA]
selectNewGeneration xs | length xs <= Config.maxPopulation = xs
                       | otherwise = take Config.maxPopulation xs
