module Test where

import Utils (duplicate, replace, delete, extract, insert)
import Gene
import Memory (Memory(..), movMpForward, movMpBackward, add)
import Program (Program(..), Operation(..), movPpForward, movPpBackward, jmpFwd, jmpBack)

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

        , ("Program.movPpBackward - middle", movPpBackward (Program (map Op [INC, FWD]) (Op DEC) (map Op [BACK, PREV, INC])) == Program (map Op [FWD]) (Op INC) (map Op [DEC, BACK, PREV, INC]))
        , ("Program.movPpBackward - start ", movPpBackward (Program [] (Op DEC) (map Op [BACK, PREV, INC])) == Program [] Terminator (map Op [DEC, BACK, PREV, INC]))
        , ("Program.movPpBackward - end   ", movPpBackward (Program (map Op [INC, FWD]) (Op DEC) []) == Program (map Op [FWD]) (Op INC) (map Op [DEC]))
        ]
