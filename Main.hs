module Main where

import System.Environment (getArgs)
import Mutator (DNA (..), iterateDna)

main :: IO ()
main = do   args <- getArgs
            interact (process $ parseArgs args)

data Args = Args {iterations :: Int}

parseArgs :: [String] -> Args
parseArgs ss = Args {iterations = read $ head ss}

process :: Args -> String -> String
process args = writeDna . iterateDna (iterations args) . readDna

readDna :: String -> DNA
readDna s = DNA $ read s 

writeDna :: DNA -> String
writeDna l = dnaToString l
    where dnaToString (DNA x) = show x
