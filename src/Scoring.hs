module Scoring (Score, score) where

import VM (VM(..))
import Memory (Memory(..))
import Dna (DNA(..))
import Processor (process, defaultVM)
import qualified Config

type Score = Double

score :: DNA -> Score
score dna = test result
    where   result = takeMem0 $ processInDefaultVm dna

processInDefaultVm :: DNA -> VM
processInDefaultVm = process . defaultVM

takeMem0 :: VM -> Int
takeMem0 (VM (Memory _ (x:_)) _) = x

test :: Int -> Double
test x = (fromIntegral x) / diapason

diapason :: Double
diapason = (fromIntegral (maxBound::Int)) - (fromIntegral (minBound::Int))

