module VM (VM(..), inc, dec, prev, next, fwd, back) where

import Memory (Memory(..), movMpForward, movMpBackward, add)
import Program (Program(..), Operation(..), movPpForward, movPpBackward, jmpFwd, jmpBack)

data VM = VM Memory Program

inc :: VM -> VM
inc (VM memory program) = VM (add 1 memory) (movPpForward program)

dec :: VM -> VM
dec (VM memory program) = VM (add (-1) memory) (movPpForward program)

prev :: VM -> VM
prev (VM memory program) = VM (movMpBackward memory) (movPpForward program)

next :: VM -> VM
next (VM memory program) = VM (movMpForward memory) (movPpForward program)

fwd :: VM -> VM
fwd (VM memory program) = VM memory (jmpFwd program)

back :: VM -> VM
back (VM memory program) = VM memory (jmpBack program)

