module Gene where 

data Gene = INC | DEC | PREV | NEXT | FWD | BACK deriving (Read, Show, Eq)
