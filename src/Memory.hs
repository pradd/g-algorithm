module Memory (Memory(..), movMpForward, movMpBackward, add) where

import Utils (replace)

data Memory = Memory MemPointer [Int] deriving (Eq, Show)

type MemPointer = Int

movMpForward :: Memory -> Memory
movMpForward (Memory pointer memory) | pointer == length memory - 1 = (Memory 0 memory)
                                     | otherwise                    = (Memory (pointer + 1) memory)

movMpBackward :: Memory -> Memory
movMpBackward (Memory pointer memory) | pointer == 0 = (Memory (length memory - 1) memory)
                                      | otherwise    = (Memory (pointer - 1) memory)

add :: Int -> Memory -> Memory
add val (Memory i memory) = Memory i updatedMemory
    where   updatedMemory = replace i ((memory !! i) + val) memory


