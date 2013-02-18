module VM (VM(..), inc, dec, prev, next, fwd, back) where

import qualified Config
import Utils (replace)

data VM = VM Memory Program
data Memory = Memory MemPointer [Int]
data Program = Program OpPointer [Operation]

type MemPointer = Int
type OpPointer = Int

movMpForward :: Memory -> Memory
movMpForward (Memory pointer memory) | pointer == length memory - 1 = (Memory 0 memory)
                                     | otherwise                    = (Memory (pointer + 1) memory)

movMpBackward :: Memory -> Memory
movMpBackward (Memory pointer memory) | pointer == 0 = (Memory (length memory - 1) memory)
                                      | otherwise    = (Memory (pointer - 1) memory)

add :: Int -> Memory -> Memory
add val (Memory i memory) = Memory i updatedMemory
    where   updatedMemory = replace i ((memory !! i) + val) memory

inc :: VM -> VM
inc = (VM memory program) = VM (add 1 memory) program

dec :: VM -> VM
dec = (VM memory program) = VM (add (-1) memory) program

prev :: VM -> VM
prev (VM memory program) = VM (movMpBackward memory) program

next :: VM -> VM
next (VM memory program) = VM (movMpForward memory) program

fwd :: VM -> VM
fwd = id

back :: VM -> VM
back = id
