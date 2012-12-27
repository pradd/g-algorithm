module Main where

import Mutator (DNA (..), iterateDna)

main :: IO ()
main = interact process

process :: String -> String
process = writeDna . iterateDna 5 . readDna

readDna :: String -> DNA
readDna s = DNA $ read s 

writeDna :: DNA -> String
writeDna l = dnaToString l
    where dnaToString (DNA x) = show x
