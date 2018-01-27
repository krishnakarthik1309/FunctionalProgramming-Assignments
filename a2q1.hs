coinChange :: Int -> [Int] -> [Int]
coinChange a (x:xs) = minCoins a (x:xs) [0 | z <- [1..(1 + length xs)]]

minCoins :: Int -> [Int] -> [Int] -> [Int]
minCoins 0 (x:xs) (c:cs) = (c:cs)
minCoins a (x:xs) (c:cs)    | length xs > 0 && a - x < 0    = (c:minCoins a xs cs)
                            | length xs > 0 && a - x >= 0   = getSmall (minCoins (a - x) (x:xs) (c+1:cs), (c:minCoins a xs cs))
                            | otherwise                     = minCoins (a - x) (x:xs) (c+1:cs)

getSmall :: ([Int], [Int]) -> [Int]
getSmall (xs, ys)   | sum xs <= sum ys  = xs
                    | otherwise         = ys