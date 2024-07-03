import Control.Monad

data State s a = State (s -> (a,s))

runState (State f) s = f s

put :: s -> State s ()
put state = State (\oldState -> ((), state))

get :: State s s
get = State (\state -> (state,state))

modify :: (s -> s) -> State s ()
modify f = State (\state -> ((), f state))

-- The following 3 rows fail:
instance Functor (State s) where
  fmap f (State s) = State (f x)

-- The following 3 rows fail:
instance Applicative State where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = State (\s -> (x,s))

  op >>= f = State h
    where h state0 = let (val,state1) = runState op state0
                         op2 = f val
                     in runState op2 state1

add :: Int -> State Int ()
add i = do old <- get
           put (old+i)

main = do
  print(runState (add 1 >> add 3 >> add 5 >> add 6) 0)