module Processor (process) where

import Dna (DNA(..), Gene, translate)

process :: Int -> DNA -> Int
process x (DNA dna) = foldl performOp x dna

performOp :: Int -> Gene -> Int
performOp x gene = (translate gene) x
