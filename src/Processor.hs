module Processor (process) where

import Dna

process :: Int -> DNA -> Int
process x (DNA dna) = foldl performOp x dna

performOp :: Int -> Gene -> Int
performOp x ADD1 = x + 1
performOp x ADD5 = x + 5
performOp x SUB1 = x - 1
performOp x SUB5 = x - 5
