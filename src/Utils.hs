module Utils (duplicate, replace, delete, extract, insert, change) where

duplicate :: Int -> Int -> [a] -> [a]
duplicate start end xs = headSequence (start-1) xs ++ duplicatedSequence start end xs ++ tailSequence end xs

headSequence :: Int -> [a] -> [a] 
headSequence index dna = take index dna

duplicatedSequence :: Int -> Int -> [a] -> [a] 
duplicatedSequence start end xs = extract' ++ extract'
    where   extract' = extract start end xs

tailSequence :: Int -> [a] -> [a]
tailSequence index xs = drop index xs 

replace :: Int -> a -> [a] -> [a]
replace i x xs = headSequence i xs ++ [x] ++ tailSequence (i+1) xs

delete :: Int -> Int -> [a] -> [a]
delete start end xs = headSequence (start-1) xs ++ tailSequence end xs

extract :: Int -> Int -> [a] -> [a]
extract start end xs = take (end-start+1) (drop (start-1) xs)

insert :: Int -> [a] -> [a] -> [a]
insert i what to = headSequence (i-1) to ++ what ++ tailSequence (i-1) to

change :: Int -> a -> [a] -> [a]
change i val xs = replace i ((xs !! i) + val) xs