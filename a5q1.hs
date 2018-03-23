data Tree a = Node (Tree a) a (Tree a) | Null

height :: Tree a -> Int
height Null = 0
height (Node lTree _ rTree) = 1 + (max (height lTree) (height rTree))

dia :: Tree a -> Int
dia Null = 0
dia (Node lTree _ rTree) = max (1 + height lTree + height rTree) (max (dia lTree) (dia rTree))