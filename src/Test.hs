module Test where

import Utils (duplicate, replace, delete, extract, insert)

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
        ]

