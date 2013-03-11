module Processor (process, defaultVM) where

import Data.List (replicate)

import Program
import Memory
import Dna (DNA(..), opsMap)
import VM (VM(..), inc, dec, prev, next, fwd, back)
import qualified Config

process :: VM -> VM
process vm = process' Config.maxCycle vm
        where   process' _ vm'@(VM _ (Program _ Terminator _)) = vm'
                process' 0 vm'' = vm''
                process' iteration vm''' = process' (iteration - 1) (process'' vm''')
                process'' vm@(VM _ (Program _ (Op gene) _)) = (get gene opsMap) vm

get :: Eq a => a -> [(a, b)] -> b
get key xs = snd $ head $ filter (\x->fst x==key) xs

defaultVM :: DNA -> VM
defaultVM dna = VM defaultMemory (defaultProgram dna)

defaultMemory = Memory 0 $ replicate Config.memSize 0
defaultProgram (DNA (d:dx)) = Program [] (Op d) (map Op dx)
