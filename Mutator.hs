module Mutator (DNA (..), iterateDna) where

data DNA = DNA Int

iterateDna :: Int -> DNA -> DNA
iterateDna i dna = (iterate (evolveDna . mutateDna) dna) !! i

mutateDna :: DNA -> [DNA]
mutateDna (DNA x) = [DNA (x - 1), DNA x, DNA (x + 1)]

evolveDna :: [DNA] -> DNA
evolveDna xs = foldl1 fight xs 

fight :: DNA -> DNA -> DNA
fight dna1 dna2 | proximity dna1 <= proximity dna2  = dna1 
                | otherwise                         = dna2 

type Score = Int

proximity :: DNA -> Score
proximity (DNA x) = abs (x - 0)
