module Main where

import System.Environment (getArgs)

import Dna (DNA(..))
import Evaluator (evaluate)

main :: IO ()
main = do   args <- getArgs
            interact (process $ parseArgs args)

data Args = Args {iterations :: Int}

parseArgs :: [String] -> Args
parseArgs ss = Args {iterations = read $ head ss}

process :: Args -> String -> String
process args = writeDna . evaluate (iterations args) . readDna

readDna :: String -> [DNA]
readDna s = map (DNA . read) (lines s) 

writeDna :: [DNA] -> String
writeDna dnas = unlines $ map dnaToString dnas
    where dnaToString (DNA x) = show x
