import Data.List  -- DO NOT REMOVE

-- top level function must be called
-- genWBPS

genWBPS :: Int -> [[Char]]
genWBPS n
    | n == 0 = [""]
    | n == 1 = ["()"]
    | otherwise = ["(" ++ a ++ ")" ++ b | x <- [0.. n-1], a <- genWBPS x, b <- genWBPS (n - 1- x)]