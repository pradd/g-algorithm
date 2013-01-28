module Config where

import Dna

maxPopulation = 100 :: Int
fertility = 3 :: Int
iterations = 5 :: Int

score :: DNA -> Score
score (DNA x) = abs x




