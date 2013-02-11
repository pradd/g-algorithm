module Mutator (mutateDna) where

import System.Random (StdGen, randomR, randomRs)

import Dna (DNA(..), genes)
import Utils (duplicate, replace, delete, extract, insert)

mutateDna :: (StdGen, DNA) -> (StdGen, DNA)
mutateDna (rnd, dna) = (mutations !! mutationIndex) seed dna
    where   (mutationIndex, seed) = randomR mutationsRange rnd
            mutationsRange = (0, (length mutations) -1)

mutations :: [StdGen -> DNA -> (StdGen, DNA)]
mutations = [ noMutation
            , duplicateMutation
            , atomicMutation
            , deleteMutation
            , moveMutation
            ]

noMutation :: StdGen -> DNA -> (StdGen, DNA)
noMutation rnd dna = (rnd, dna)

duplicateMutation :: StdGen -> DNA -> (StdGen, DNA)
duplicateMutation rnd (DNA dna) = (rnd'', DNA $ duplicate start end dna)
    where       (index1, rnd') = randomR range rnd
                (index2, rnd'') = randomR range rnd'
                range = (1, (length dna))
                start = min index1 index2
                end = max index1 index2

atomicMutation :: StdGen -> DNA -> (StdGen, DNA)
atomicMutation rnd (DNA dna) = (rnd'', DNA $ replace i newGene dna)
    where       (i, rnd') = randomR (0, (length dna)-1) rnd
                newGene = genes !! j
                (j, rnd'') = randomR (0, (length genes)-1) rnd'

moveMutation :: StdGen -> DNA -> (StdGen, DNA)
moveMutation rnd (DNA dna) = (rnd''', DNA $ insert destination transposone transitionalDna)
    where       (index1, rnd') = randomR (1, (length dna)) rnd
                (index2, rnd'') = randomR (1, (length dna)) rnd'
                start = min index1 index2
                end = max index1 index2
                transposone = extract start end dna
                transitionalDna = delete start end dna
                (destination, rnd''') = randomR (1, (length transitionalDna)) rnd''

deleteMutation :: StdGen -> DNA -> (StdGen, DNA)
deleteMutation rnd (DNA dna) = (rnd'', DNA $ safeDelete start end dna)
    where       (index1, rnd') = randomR range rnd
                (index2, rnd'') = randomR range rnd'
                range = (1, (length dna))
                start = min index1 index2
                end = max index1 index2

safeDelete :: Int -> Int -> [a] -> [a]
safeDelete start end xs | length xs == (end - start + 1) = xs
                        | otherwise = delete start end xs
