data Nat = Zero | Succ Nat

ident :: Nat -> Nat
ident n = n

foldn :: (a -> a) -> a -> Nat -> a
foldn f id Zero     = id
foldn f id (Succ n) = f (foldn f id n)

oddnat :: Nat -> Bool
oddnat Zero             = False
oddnat n                = if mod (foldn (1+) 0 n) 2 == 1
                            then True
                            else False

splFunc :: (Nat, Bool) -> (Nat, Bool)
splFunc (a, b)       = (if b == False then Succ a else ident a, not b)

halfnat :: Nat -> Nat
halfnat Zero            = Zero
halfnat n               = fst (foldn splFunc (Zero, False) n)

succShow nString = if nString == "Zero"
                then "Succ Zero"
                else "Succ (" ++ nString ++ ")"

instance Show Nat where
    show Zero       = "Zero"
    show n          = foldn succShow "Zero" n