module Dna (DNA (..), Score, genes) where 

import Gene (Gene(..))
import VM (VM, inc, dec, prev, next, fwd, back)

data DNA = DNA [Gene]

--translate :: Gene -> Int -> Int
--translate gene = snd $ head $ filter ((gene ==) . fst) opsMap

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
