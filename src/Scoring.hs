module Scoring 
    ( Score
    , score
    , testX
    , setMemIx
    ) where

import VM (VM(..))
import Memory (Memory(..))
import Dna (DNA(..))
import Processor (process, defaultVM)
import Utils (replace)

type Score = Double

score :: DNA -> Score
score dna = (test1 dna) * (test2 dna)

setMemIx :: Int -> Int -> VM -> VM
setMemIx i x (VM (Memory mi xs) p) = VM (Memory mi (replace i x xs)) p

takeMemIx :: Int -> VM -> Int
takeMemIx i (VM (Memory _ xs) _) = xs !! i

proximity :: Int -> Int -> Score
proximity expected actual = projectToDiapason $ (maxBound::Int) - (abs $ expected - actual)

testX :: Int -> DNA -> Score
testX x dna = proximity x $ takeMemIx 1 $ process $ setMemIx 0 x $ defaultVM dna 

test1 :: DNA -> Score
test1 dna = testX 3 dna

test2 :: DNA -> Score
test2 dna = testX 5 dna

projectToDiapason :: Int -> Double
projectToDiapason x = 0.5 + ((fromIntegral x) / diapason)

diapason :: Double
diapason = (fromIntegral (maxBound::Int)) - (fromIntegral (minBound::Int))

