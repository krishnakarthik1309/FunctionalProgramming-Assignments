import Control.Monad.ST.Lazy hiding (State)
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

-- https://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative
-- Krishna(14292): The following 5 lines are implemented from above link
instance Functor StateMonad where
  fmap = liftM

instance Applicative StateMonad where
  pure  = return
  (<*>) = ap

data Exp = Con Int -- integer constant
         | Var Var -- variable
         | Add Exp Exp -- e1 + e2
         | Mul Exp Exp -- e1 * e2
         | Sub Exp Exp -- e1 - e2
         | Div Exp Exp -- e1 `div` e2 (Integer divison)
         | Neg Exp -- (-e)
         | PP Var -- ++v
         | Assign Var Exp -- v = e

type Var = String
type State = Var -> Int

data StateMonad a = SM (State -> (a, State))

instance Monad StateMonad where
    return i = SM (\st -> (i,st))
    (SM sx) >>= k = SM sx1
       where sx1 = \st -> let (i1, st1) = sx st
                              SM sx11 = k i1
                          in sx11 st1

initState :: State
initState = \v -> error ("variable " ++ ['"'] ++ v ++ ['"']  ++ " is undefined.")

eval :: Exp -> StateMonad Int
eval (Con i)      = return i
eval (Var v)      = SM (\s -> (s v, s))
eval (Add e1 e2)  = do  i1 <- eval e1
                        i2 <- eval e2
                        return (i1 + i2)
eval (Mul e1 e2)  = do  i1 <- eval e1
                        i2 <- eval e2
                        return (i1 * i2)
eval (Sub e1 e2)  = do  i1 <- eval e1
                        i2 <- eval e2
                        return (i1 - i2)
eval (Div e1 e2)  = do  i1 <- eval e1
                        i2 <- eval e2
                        if (i2 == 0) then error "Division by 0."
                        else return (i1 `div` i2)
eval (Neg e1   )  = do  i1 <- eval e1
                        return (0 - i1)
eval (PP v) =       do  SM (\s ->   let i  = 1 + s v
                                    in  (i, \v1 -> if (v == v1) then i else s v1 ))
eval (Assign v e1) = do i <- eval e1
                        SM (\s -> (i, \v1 -> if (v == v1) then i else s v1))

interpret pgm = let  (SM sx) = eval pgm
                in fst (sx initState)