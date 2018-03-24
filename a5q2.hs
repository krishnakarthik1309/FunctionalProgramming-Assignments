data Gtree a = Gnode a [Gtree a]

data Btree a = Bnode (Btree a) (Btree a) | Leaf a

instance (Show a) => Show (Gtree a) where
    show (Gnode a []) = "Gnode " ++ show a ++ " []"
    show (Gnode a ys) = "Gnode " ++ show a ++ " " ++ show ys ++ ""

instance (Show a) => Show (Btree a) where
    show (Leaf a) = "Leaf " ++ show a
    show (Bnode bt1 bt2) = "Bnode (" ++ show bt1 ++ ") " ++ "(" ++ show bt2 ++ ")" 

instance (Eq a) => Eq (Gtree a) where
    (Gnode f1 args1) == (Gnode f2 args2)    = (f1 == f2) && (args1 == args2)

instance (Eq a) => Eq (Btree a) where
    (Leaf l1) == (Leaf l2)                  = l1 == l2
    (Bnode bt1 bt2) == (Bnode bt3 bt4)      = (bt1 == bt3) && (bt2 == bt4)
    
g2b :: Gtree a -> Btree a
g2b (Gnode f []) = Leaf f
g2b (Gnode f args) = Bnode (g2b (Gnode f (take (length args - 1) args))) (g2b (last args))

gCombine :: Gtree a -> Gtree a -> Gtree a
gCombine (Gnode f1 args1) (Gnode f2 args2) = Gnode f1 (args1 ++ [Gnode f2 args2])

b2g :: Btree a -> Gtree a
b2g (Leaf f) = Gnode f []
b2g (Bnode bt1 bt2) = gCombine (b2g bt1) (b2g bt2)