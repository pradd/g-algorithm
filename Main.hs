module Main where

import System.Environment (getArgs)
import System.Random (StdGen, newStdGen)

import Dna (DNA(..))
import Evaluator (evaluate)

main :: IO ()
main = do   args <- getArgs
            randomGen <- newStdGen
            interact (process randomGen $ parseArgs args)

data Args = Args {iterations :: Int}

parseArgs :: [String] -> Args
parseArgs ss = Args {iterations = read $ head ss}

process :: StdGen -> Args -> String -> String
process rnd args = writeDna . evaluate rnd (iterations args) . readDna

readDna :: String -> [DNA]
readDna s = map (DNA . read) (lines s) 

writeDna :: [DNA] -> String
writeDna dnas = unlines $ map dnaToString dnas
    where dnaToString (DNA x) = show x
