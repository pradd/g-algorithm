module Config where

import Dna
import Processor (process)

maxPopulation = 100 :: Int
fertility = 3 :: Int
iterations = 5 :: Int

memSize = 100 :: Int

score :: DNA -> Score
score dna = theHigherTheBetter dna


theHigherTheBetter dna = - (process 0 dna)

asCloseToZeroAsPossible dna =   abs (5 -  process 5  dna)
                              + abs (10 - process 10 dna)




