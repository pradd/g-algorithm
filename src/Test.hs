module Test where

import Utils (duplicate, replace)

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
        ]

