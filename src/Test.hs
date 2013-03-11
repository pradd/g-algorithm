module Test where

import Utils (duplicate, replace, delete, extract, insert)
import Gene
import Memory (Memory(..), movMpForward, movMpBackward, add)
import Program (Program(..), Operation(..), movPpForward, movPpBackward, jmpFwd, jmpBack)
import VM (VM(..), inc, dec, prev, next, fwd, back)

main = putStrLn $ unlines $ map test tests

test (desc, predicate) | predicate == True = desc ++ "  OK"
                       | otherwise         = desc ++ "  FAILED"

tests = [ ("duplicate - happy path     ", duplicate 1 2 [1,2,3] == [1,2,1,2,3]              )
        , ("duplicate - start-start    ", duplicate 1 1 [1,2,3] == [1,1,2,3]                )
        , ("duplicate - end - end      ", duplicate 3 3 [1,2,3] == [1,2,3,3]                )
        , ("duplicate - full           ", duplicate 1 3 [1,2,3] == [1,2,3,1,2,3]            )
        , ("duplicate - minimal list   ", duplicate 1 1 [1] == [1,1]                        )

        , ("replace - happy path       ", replace 1 5 [1,2,3] == [1,5,3]                    )
        , ("replace - lower bound      ", replace 0 5 [1,2,3] == [5,2,3]                    )
        , ("replace - upper bound      ", replace 2 5 [1,2,3] == [1,2,5]                    )
        , ("replace - minimal list     ", replace 0 5 [1] == [5]                            )

        , ("delete - happy path        ", delete 3 5 [1,2,3,4,5,6] == [1,2,6]               )
        , ("delete - lower bound       ", delete 1 2 [1,2,3,4,5,6] == [3,4,5,6]             )
        , ("delete - upper bound       ", delete 3 6 [1,2,3,4,5,6] == [1,2]                 )
        , ("delete - full delete       ", delete 1 6 [1,2,3,4,5,6] == []                    )
        , ("delete - single delete     ", delete 4 4 [1,2,3,4,5,6] == [1,2,3,5,6]           )
        , ("delete - minimal list      ", delete 1 1 [1] == []                              )
       
        , ("extract - happy path       ", extract 2 5 [1,2,3,4,5,6] == [2,3,4,5]            )
        , ("extract - lower bound      ", extract 1 3 [1,2,3,4,5,6] == [1,2,3]              )
        , ("extract - upper bound      ", extract 4 6 [1,2,3,4,5,6] == [4,5,6]              )
        , ("extract - minimal list     ", extract 1 1 [1] == [1]                            )
        , ("extract - full list        ", extract 1 6 [1,2,3,4,5,6] == [1,2,3,4,5,6]        )

        , ("insert - happy path        ", insert 2 [2,3] [1,4,5,6] == [1,2,3,4,5,6]         )
        , ("insert - lower bound       ", insert 1 [1,2,3] [4,5,6] == [1,2,3,4,5,6]         )
        , ("insert - upper bound       ", insert 6 [6] [1,2,3,4,5] == [1,2,3,4,5,6]         )
        , ("insert - minimal list      ", insert 1 [1] [] == [1]                            )
        , ("insert - full list         ", insert 4 [] [1,2,3,4,5,6] == [1,2,3,4,5,6]        )
  
        , ("Memory.add - middle        ", add 1 (Memory 2 [0,1,2,3]) == Memory 2 [0,1,3,3]   )
        , ("Memory.add - lower bound   ", add 1 (Memory 0 [0,1]) == Memory 0 [1,1]           )
        , ("Memory.add - upper bound   ", add 1 (Memory 3 [0,1,2,3]) == Memory 3 [0,1,2,4]   )
        , ("Memory.add - decrement     ", add (-1) (Memory 2 [0,1,2,3]) == Memory 2 [0,1,1,3])

        , ("Memory.movMpForward - middle ", movMpForward (Memory 1 [0,1,2]) == Memory 2 [0,1,2])
        , ("Memory.movMpForward - start  ", movMpForward (Memory 0 [0,1,2]) == Memory 1 [0,1,2])
        , ("Memory.movMpForward - end    ", movMpForward (Memory 2 [0,1,2]) == Memory 0 [0,1,2])

        , ("Memory.movMpBackward - middle", movMpBackward (Memory 1 [0,1,2]) == Memory 0 [0,1,2])
        , ("Memory.movMpBackward - start ", movMpBackward (Memory 0 [0,1,2]) == Memory 2 [0,1,2])
        , ("Memory.movMpBackward - end   ", movMpBackward (Memory 2 [0,1,2]) == Memory 1 [0,1,2])

        , ("Program.movPpForward - middle    ", movPpForward (Program (map Op [INC, FWD]) (Op DEC) (map Op [BACK, PREV, INC])) == Program (map Op [DEC, INC, FWD]) (Op BACK) (map Op [PREV, INC]))
        , ("Program.movPpForward - start     ", movPpForward (Program [] (Op DEC) (map Op [BACK, PREV, INC])) == Program [Op DEC] (Op BACK) (map Op [PREV, INC]))
        , ("Program.movPpForward - end       ", movPpForward (Program (map Op [INC, FWD]) (Op DEC) []) == Program (map Op [DEC, INC, FWD]) Terminator [])
        , ("Program.movPpForward - terminator", movPpForward (Program [Op INC] Terminator [Op DEC]) == Program [Op INC] Terminator [Op DEC])

        , ("Program.movPpBackward - middle    ", movPpBackward (Program (map Op [INC, FWD]) (Op DEC) (map Op [BACK, PREV, INC])) == Program (map Op [FWD]) (Op INC) (map Op [DEC, BACK, PREV, INC]))
        , ("Program.movPpBackward - start     ", movPpBackward (Program [] (Op DEC) (map Op [BACK, PREV, INC])) == Program [] Terminator (map Op [DEC, BACK, PREV, INC]))
        , ("Program.movPpBackward - end       ", movPpBackward (Program (map Op [INC, FWD]) (Op DEC) []) == Program (map Op [FWD]) (Op INC) (map Op [DEC]))
        , ("Program.movPpBackward - terminator", movPpBackward (Program [Op INC] Terminator [Op DEC]) == Program [Op INC] Terminator [Op DEC])

        , ("Program.jmpFwd - middle         ", jmpFwd (Program (map Op [INC, FWD]) (Op DEC) (map Op [BACK, PREV, INC])) == Program (map Op [BACK, DEC, INC, FWD]) (Op PREV) (map Op [INC]))
        , ("Program.jmpFwd - inner brackets ", jmpFwd (Program (map Op [FWD]) (Op DEC) (map Op [FWD, INC, BACK, PREV, BACK, INC])) == Program (map Op [BACK, PREV, BACK, INC, FWD, DEC, FWD]) (Op INC) [])
        , ("Program.jmpFwd - terminator     ", jmpFwd (Program (map Op [INC, FWD]) (Op DEC) [Op FWD]) == Program (map Op [FWD, DEC, INC, FWD]) Terminator [])

        , ("Program.jmpBack - middle         ", jmpBack (Program (map Op [INC, FWD]) (Op DEC) (map Op [BACK, PREV, INC])) == Program [] (Op FWD) (map Op [INC, DEC, BACK, PREV, INC]))
        , ("Program.jmpBack - inner brackets ", jmpBack (Program (map Op [BACK, INC, FWD, DEC, FWD]) (Op BACK) (map Op [INC])) == Program [] (Op FWD) (map Op [DEC, FWD, INC, BACK, BACK, INC]))
        , ("Program.jmpBack - terminator     ", jmpBack (Program (map Op [INC]) (Op BACK) [Op FWD]) == Program [] Terminator (map Op [INC, BACK, FWD]))

        , ("VM.inc                     ", inc (VM (Memory 0 [5]) (Program [] (Op INC) [])) == VM (Memory 0 [6]) (Program [Op INC] Terminator []))
        , ("VM.dec                     ", dec (VM (Memory 0 [5]) (Program [] (Op DEC) [])) == VM (Memory 0 [4]) (Program [Op DEC] Terminator []))
        , ("VM.prev                    ", prev (VM (Memory 1 [0,1,2]) (Program [] (Op PREV) [])) == VM (Memory 0 [0,1,2]) (Program [Op PREV] Terminator []))
        , ("VM.next                    ", next (VM (Memory 0 [0,1,2]) (Program [] (Op NEXT) [])) == VM (Memory 1 [0,1,2]) (Program [Op NEXT] Terminator []))
        , ("VM.fwd - rewind            ", fwd (VM (Memory 0 [0]) (Program [] (Op FWD) (map Op [BACK, PREV]))) == VM (Memory 0 [0]) (Program [Op BACK, Op FWD] (Op PREV) []))
        , ("VM.fwd - into loop         ", fwd (VM (Memory 0 [1]) (Program [] (Op FWD) (map Op [INC, PREV]))) == VM (Memory 0 [1]) (Program [Op FWD] (Op INC) [Op PREV]))
        , ("VM.back                    ", back (VM (Memory 0 [0]) (Program (map Op [PREV, FWD]) (Op BACK) [])) == VM (Memory 0 [0]) (Program [] (Op FWD) [Op PREV, Op BACK]))
        ]
