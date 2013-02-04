module Dna (DNA (..), Gene (..), Score, translate, genes) where 

data DNA = DNA [Gene]
data Gene = ADD1 | SUB1 | ADD5 | SUB5 deriving (Read, Show, Eq)

translate :: Gene -> Int -> Int
translate gene = snd $ head $ filter ((gene ==) . fst) opsMap

genes :: [Gene]
genes = map fst opsMap

opsMap :: [(Gene, Int -> Int)]
opsMap = [ (ADD1, \x -> x + 1)
         , (SUB1, \x -> x - 1)
         , (ADD5, \x -> x + 5)
         , (SUB5, \x -> x - 5)
         ] 

type Score = Int
