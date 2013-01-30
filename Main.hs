module Main where

import System.Random (StdGen, newStdGen)

import Dna (DNA(..))
import Evolver (evolve)

main :: IO ()
main = do   rnd <- newStdGen
            interact (process rnd)

process :: StdGen -> String -> String
process rnd = writeDna . evolve rnd . readDna

readDna :: String -> [DNA]
readDna s = map (DNA . toOps) (lines s)
    where toOps l = map read $ words l

writeDna :: [DNA] -> String
writeDna dnas = unlines $ map dnaToString dnas
    where dnaToString (DNA x) = unwords $ map show x
