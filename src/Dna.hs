module Dna (DNA (..), Gene (..), Score, translate, genes) where 

import VM (VM, inc, dec, prev, next, fwd, back)

data DNA = DNA [Gene]
data Gene = INC | DEC | PREV | NEXT | FWD | BACK deriving (Read, Show, Eq)

translate :: Gene -> Int -> Int
translate gene = snd $ head $ filter ((gene ==) . fst) opsMap

genes :: [Gene]
genes = map fst opsMap

opsMap :: [(Gene, VM -> VM)]
opsMap = [ (INC, inc)
         , (DEC, dec)
         , (PREV, prev)
         , (NEXT, next)
         , (FWD, fwd)
         , (BACK, back)
         ] 



type Score = Int
