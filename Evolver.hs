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
evaluate rnd i xdna = evaluate seed (i-1) $ liveCycle seeds xdna
    where  (seed:seeds) = splitRandomGens rnd

splitRandomGens ::  StdGen -> [StdGen]
splitRandomGens rnd = iterate (fst . split) rnd

liveCycle :: [StdGen] -> [DNA] -> [DNA]
liveCycle seeds dnas = filterByScore $ concatMap breedPopulation dnas
    where   breedPopulation :: DNA -> [(DNA, Score)]
            breedPopulation dna = map (calcScore . mutateDna) $ zip seeds $ breed dna

breed :: DNA -> [DNA]
breed = replicate Config.fertility

calcScore :: DNA -> (DNA, Score)
calcScore d = (d, Config.score d)

filterByScore :: [(DNA, Score)] -> [DNA]
filterByScore ps = selectNewGeneration $ map fst $ sortBy (compare `on` snd) ps

selectNewGeneration :: [DNA] -> [DNA]
selectNewGeneration xs | length xs <= Config.maxPopulation = xs
                       | otherwise = take Config.maxPopulation xs
