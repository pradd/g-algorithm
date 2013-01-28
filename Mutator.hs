module Mutator (mutateDna) where

import System.Random (StdGen, randomR)

import Dna

mutateDna :: (StdGen, DNA) -> DNA
mutateDna (rnd, (DNA x)) = DNA (x + r)
    where   r = fst $ randomR (-5, 5) rnd  
