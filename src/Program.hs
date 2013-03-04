module Program (Program(..), Operation(..), movPpForward, movPpBackward, jmpFwd, jmpBack) where

import Gene (Gene(..))

data Program = Program [Operation] Operation [Operation] deriving (Eq)
data Operation = Op Gene | Terminator deriving (Eq)

movPpForward :: Program -> Program
movPpForward (Program prevOps op nextOps) = Program (push' op prevOps) (head' nextOps) (tail' nextOps)

movPpBackward :: Program -> Program
movPpBackward = reverse' . movPpForward . reverse'

head' :: [Operation] -> Operation
head' [] = Terminator
head' (x:_) = x

tail' :: [Operation] -> [Operation]
tail' [] = []
tail' (_:xs) = xs

push' :: Operation -> [Operation] -> [Operation]
push' Terminator ops = ops
push' op ops = op:ops

reverse' :: Program -> Program
reverse' (Program prevOps op nextOps) = Program nextOps op prevOps

jmpFwd :: Program -> Program
jmpFwd p = movPpForward $ traverse (Op BACK) (Op FWD) 0 $ movPpForward p

jmpBack :: Program -> Program
jmpBack p = movPpForward $ reverse' $ traverse (Op FWD) (Op BACK) 0 $ reverse' $ movPpBackward p

traverse :: Operation -> Operation -> Int -> Program -> Program
traverse _ _ _ p@(Program _ Terminator _) = p
traverse endingOp _ 0 p@(Program _ op _) | op == endingOp = p
traverse endingOp openingOp stackDepth p@(Program prevOps op nextOps)       | op == openingOp = traverse endingOp openingOp (stackDepth + 1) (movPpForward p)
                                                                            | op == endingOp  = traverse endingOp openingOp (stackDepth - 1) (movPpForward p)
                                                                            | otherwise       = traverse endingOp openingOp stackDepth       (movPpForward p)
