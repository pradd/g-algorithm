module Main where

import System.Random (StdGen, newStdGen)

import Dna (DNA(..))
import Evaluator (evolve)

main :: IO ()
main = do   rnd <- newStdGen
            interact (process rnd)

process :: StdGen -> String -> String
process rnd = writeDna . evolve rnd . readDna

readDna :: String -> [DNA]
readDna s = map (DNA . read) (lines s) 

writeDna :: [DNA] -> String
writeDna dnas = unlines $ map dnaToString dnas
    where dnaToString (DNA x) = show x
