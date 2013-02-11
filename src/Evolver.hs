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
evaluate rnd i xdna = evaluate rnd' (i-1) dnas
    where  (rnd', dnas) = liveCycle rnd xdna

liveCycle :: StdGen -> [DNA] -> (StdGen, [DNA])
liveCycle rnd dnas = (rnd', dnas')
    where   (rnd', xs) = breedPopulation' rnd dnas
            dnas' = filterByScore xs

breedPopulation' :: StdGen -> [DNA] -> (StdGen, [(DNA, Score)])
breedPopulation' rnd []     = (rnd, [])
breedPopulation' rnd (x:xs) = (rnd'', x' ++ xs')
    where   (rnd', x')   = breedPopulation rnd x
            (rnd'', xs') = breedPopulation' rnd' xs      


breedPopulation :: StdGen -> DNA -> (StdGen, [(DNA, Score)])
breedPopulation rnd dna = (rnd', map calcScore mutated)
    where   (rnd', mutated) = iterateMutateDna rnd $ breed dna 

iterateMutateDna :: StdGen -> [DNA] -> (StdGen, [DNA])
iterateMutateDna rnd []     = (rnd , []) 
iterateMutateDna rnd (x:xs) = (rnd'', dna : dnas)
    where   (rnd' , dna ) = mutateDna (rnd, x)
            (rnd'', dnas) = iterateMutateDna rnd' xs

breed :: DNA -> [DNA]
breed = replicate Config.fertility

calcScore :: DNA -> (DNA, Score)
calcScore d = (d, Config.score d)

filterByScore :: [(DNA, Score)] -> [DNA]
filterByScore ps = selectNewGeneration $ map fst $ sortBy (compare `on` snd) ps

selectNewGeneration :: [DNA] -> [DNA]
selectNewGeneration xs | length xs <= Config.maxPopulation = xs
                       | otherwise = take Config.maxPopulation xs
