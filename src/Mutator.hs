module Mutator (mutateDna) where

import System.Random (StdGen, randomR, randomRs)

import Dna (DNA(..), genes)
import Utils (duplicate, replace, delete, extract, insert)

type Mutation = StdGen -> DNA -> DNA

mutateDna :: (StdGen, DNA) -> DNA
mutateDna (rnd, dna) = (mutations !! mutationIndex) seed dna
    where   (mutationIndex, seed) = randomR mutationsRange rnd
            mutationsRange = (0, (length mutations) -1)

mutations :: [Mutation]
mutations = [ noMutation
            , duplicateMutation
            , atomicMutation
            , deleteMutation
            , moveMutation
            ]

noMutation :: Mutation
noMutation _ = id

duplicateMutation :: Mutation
duplicateMutation rnd (DNA dna) = DNA $ duplicate start end dna
    where       (index1:index2:_) = randomRs (1, (length dna)) rnd
                start = min index1 index2
                end = max index1 index2

atomicMutation :: Mutation
atomicMutation rnd (DNA dna) = DNA $ replace i newGene dna
    where       (i, seed) = randomR (0, (length dna)-1) rnd
                newGene = genes !! j
                (j, _) = randomR (0, (length genes)-1) seed

moveMutation :: Mutation
moveMutation rnd (DNA dna) = DNA $ insert destination transposone transitionalDna
    where       (index1, seed1) = randomR (1, (length dna)) rnd
                (index2, seed2) = randomR (1, (length dna)) seed1
                start = min index1 index2
                end = max index1 index2
                transposone = extract start end dna
                transitionalDna = delete start end dna
                (destination, _) = randomR (1, (length transitionalDna)) seed2

deleteMutation :: Mutation
deleteMutation rnd (DNA dna) = DNA $ safeDelete start end dna
    where       (index1:index2:_) = randomRs (1, (length dna)) rnd
                start = min index1 index2
                end = max index1 index2

safeDelete :: Int -> Int -> [a] -> [a]
safeDelete start end xs | length xs == (end - start + 1) = xs
                        | otherwise = delete start end xs
