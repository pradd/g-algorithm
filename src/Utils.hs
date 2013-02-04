module Utils (duplicate, replace) where

duplicate :: Int -> Int -> [a] -> [a]
duplicate start end xs = headSequence (start-1) xs ++ duplicatedSequence (start-1) end xs ++ tailSequence end xs

headSequence :: Int -> [a] -> [a] 
headSequence index dna = take index dna

duplicatedSequence :: Int -> Int -> [a] -> [a] 
duplicatedSequence start end xs = extract ++ extract
    where       extract = take (end-start) (drop start xs)

tailSequence :: Int -> [a] -> [a]
tailSequence index xs = drop index xs 

replace :: Int -> a -> [a] -> [a]
replace i x xs = headSequence i xs ++ [x] ++ tailSequence (i+1) xs
