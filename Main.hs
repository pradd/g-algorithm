module Main where

main :: IO ()
main = interact (writeDna . processDna . readDna)

data DNA = DNA Int

readDna :: String -> DNA
readDna s = DNA $ read s 

processDna :: DNA -> [DNA]
processDna (DNA x) = [DNA (x - 1), DNA x, DNA (x + 1)]

writeDna :: [DNA] -> String
writeDna l = unlines $ map dnaToString l
    where dnaToString (DNA x) = show x
