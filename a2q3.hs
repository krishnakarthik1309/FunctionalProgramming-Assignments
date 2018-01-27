errno :: String -> Int
errno = wbps 0 0

wbps :: Int -> Int -> String -> Int
wbps a b [] = a + b
wbps a b (x:xs) | a <= 0 && x == ')'    = wbps a (b + 1) xs
                | a > 0  && x == ')'    = wbps (a - 1) b xs
                | otherwise             = wbps (a + 1) b xs