module Mutator (mutateDna) where

import Dna

mutateDna :: DNA -> DNA
mutateDna (DNA x) | x < 0 = DNA (x + 1)
                  | x >= 0 = DNA (x - 1)
