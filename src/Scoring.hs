module Scoring (Score, score) where

import VM (VM(..))
import Memory (Memory(..))
import Dna (DNA)
import Processor (process, defaultVM)
import qualified Config

type Score = Int

score :: DNA -> Score
score = theHigherTheBetter

theHigherTheBetter dna = negate $ takeMem0 $ process $ defaultVM dna
    where   takeMem0 (VM (Memory _ (x:_)) _) = x


