data Peano = Zero | Succ Peano  -- incomplete

int2Peano :: Integer -> Peano
int2Peano x =   if x == 0
                    then Zero
                    else Succ (int2Peano (x - 1))


peano2Int :: Peano -> Integer
peano2Int Zero = 0
peano2Int (Succ a) = 1 + peano2Int a

instance Num Peano where
    fromInteger     = int2Peano
    x + y           = int2Peano (peano2Int x + peano2Int y)
    x - y           = if peano2Int x - peano2Int y < 0
                            then Zero
                            else int2Peano (peano2Int x - peano2Int y)
    x * y           = int2Peano (peano2Int x * peano2Int y)
    abs x           = int2Peano (peano2Int x)
    signum x        = int2Peano (signum (peano2Int x))

instance Show Peano where
    show x          = if x == Zero
                            then show "Zero"
                            else show (peano2Int x)

instance Eq Peano where
    x == y          = peano2Int x == peano2Int y

instance Ord Peano where
    x > y           = peano2Int x > peano2Int y
    x < y           = not (x > y || x == y)
    x <= y          = (x == y || x < y)
    x >= y          = (x == y || x > y)