module Dna (DNA (..), Gene (..), Score) where 

data DNA = DNA [Gene]
data Gene = ADD1 | SUB1 | ADD5 | SUB5 deriving (Read, Show)

type Score = Int
