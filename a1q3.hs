myInsert y [] = [y]
myInsert y (x:xs)   | y < x     = y:x:xs
                    | otherwise = x:(myInsert y xs)

myUnion xs [] = xs
myUnion [] ys = ys
myUnion (x:xs) (y:ys)   | x < y     = x:(myUnion xs (y:ys))
                        | otherwise = y:(myUnion (x:xs) ys)


myDoubleUnion (xs:xss) yss  | length xss > 0    = xs:(myDoubleUnion xss yss)
                            | otherwise         = xs:yss

myPS [] = [[]]
myPS (x:xs) = myDoubleUnion (myPS xs) ([myInsert x ys | ys <- myPS(xs)])

myPSV xs = length (myPS2 xs)

fnn x yss = yss ++ [myInsert x ys | ys <- yss]
myPS2 xs = foldr fnn [[]] xs
