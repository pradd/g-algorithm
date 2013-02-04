module Test where

import Utils (duplicate)

main = putStrLn $ unlines $ map test tests

test (desc, predicate) | predicate == True = desc ++ "  OK"
                       | otherwise         = desc ++ "  FAILED"

tests = [ ("duplicate - happy path     ", duplicate 1 2 [1,2,3] == [1,2,1,2,3]              )
        , ("duplicate - start-start    ", duplicate 1 1 [1,2,3] == [1,1,2,3]                )
        , ("duplicate - end - end      ", duplicate 3 3 [1,2,3] == [1,2,3,3]                )
        , ("duplicate - full           ", duplicate 1 3 [1,2,3] == [1,2,3,1,2,3]            )
        , ("duplicate - minimal        ", duplicate 1 1 [1] == [1,1]                        )
        ]

