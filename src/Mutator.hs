module Mutator (mutateDna) where

import System.Random (StdGen, randomR, randomRs)

import Dna
import Utils (duplicate)

type Mutation = StdGen -> DNA -> DNA

mutateDna :: (StdGen, DNA) -> DNA
mutateDna (rnd, dna) = (mutations !! mutationIndex) seed dna
    where   (mutationIndex, seed) = randomR mutationsRange rnd
            mutationsRange = (0, (length mutations) -1)

mutations :: [Mutation]
mutations = [ noMutation
            , duplicateMutation
            ]

noMutation :: Mutation
noMutation _ = id

duplicateMutation :: Mutation
duplicateMutation rnd (DNA dna) = DNA $ duplicate start end dna
    where       (index1:index2:_) = randomRs (1, (length dna)) rnd
                start = min index1 index2
                end = max index1 index2
