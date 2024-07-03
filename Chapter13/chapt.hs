import Data.List
import qualified Data.Map as Map
import Control.Monad

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing
Just x ?> f = f x

increase :: Eq a => a -> Int -> [(a,Int)] -> Maybe [(a,Int)]
increase key val assocs =
    lookup key assocs ?>
    check ?>
    buildResult
  where check x 
           | val < x = Nothing
           | otherwise = Just x
        buildResult x = Just ((key,val) : delete (key,x) assocs)

data Logger a = Logger [String] a deriving Show

msg :: String -> Logger ()
msg s = Logger [s] ()

instance Functor Logger where
  fmap f (Logger log x) = Logger log (f x)
  
instance Applicative Logger where
  pure = return
  (<*>) = ap
  
instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la++lb) b
    where Logger lb b = f a
	
getVal :: Logger a -> a
getVal (Logger _ a) = a
getLog :: Logger a -> [String]
getLog (Logger s _) = s

nomsg :: a -> Logger a
nomsg x = return x

annotate :: String -> a -> Logger a
annotate s x = msg s >> return x

validateUser :: String -> Logger Bool
validateUser "paul.y" = annotate "Valid user" True
validateUser "ninja" = nomsg True
validateUser u = annotate ("Invalid user: " ++ u) False

checkPassword :: String -> String -> Logger Bool
checkPassword "paul.y" "muad'dib" = annotate "Password ok" True
checkPassword "ninja"  ""         = annotate "Password ok" True
checkPassword _        pass       = annotate ("Password wrong: " ++ pass) False

(#>) :: Logger a -> (a -> Logger b) -> Logger b
Logger la a #> f = let Logger lb b = f a
                   in Logger (la ++ lb) b

square :: Int -> Logger Int
square val = annotate (show val ++ "^2") (val^2)

add :: Int -> Logger Int
add val = annotate (show val ++ "+1") (val+1)

double :: Int -> Logger Int
double val = annotate (show val ++ "*2") (val*2)

compute x = do
  a <- annotate "^2" (x*x)
  b <- annotate "+1" (a+1)
  annotate "*2" (b*2)

filterLog2 :: (Show a) => (a -> Bool) -> [a] -> Logger [a]
filterLog2 f [] = return []
filterLog2 f (x:xs) 
  | f x = do msg ("keeping "++show x)
             xs' <- filterLog2 f xs
             return (x:xs')
  | otherwise = do msg ("dropping "++show x)
                   filterLog2 f xs
  
login :: String -> String -> Logger Bool
login user password = 
  validateUser user
  #>
  \valid -> if valid then checkPassword user password
                     else nomsg False

(##>) :: Logger a -> Logger b -> Logger b
Logger la _ ##> Logger lb b = Logger (la ++ lb) b

filterLog :: (Eq a, Show a) => (a -> Bool) -> [a] -> Logger [a]
filterLog f [] = nomsg []
filterLog f (x:xs) 
    | f x       = msg ("keeping " ++ show x)  ##> filterLog f xs #> (\xs' -> nomsg (x:xs'))
    | otherwise = msg ("dropping " ++ show x) ##> filterLog f xs

data Bank = Bank (Map.Map String Int) 
  deriving Show
  
deposit :: String -> Int -> Bank -> Bank
deposit accountName amount (Bank accounts) = 
  Bank (Map.adjust (\x -> x + amount) accountName accounts)
  
withdraw :: String -> Int -> Bank -> (Int,Bank)
withdraw accountName amount (Bank accounts) = 
  let balance = Map.findWithDefault 0 accountName accounts
      withdrawal = min amount balance
      newAccounts = Map.adjust (\x -> x - withdrawal) accountName accounts
  in (withdrawal, Bank newAccounts)

share :: String -> String -> String -> Bank -> Bank
share from to1 to2 bank = 
  let (amount,bank1) = withdraw from 100 bank
      half = div amount 2
      rest = amount - half
      bank2 = deposit to1 half bank1
      bank3 = deposit to2 rest bank2
  in bank3


data BankOp a = BankOp (Bank -> (a,Bank))

runBankOp :: BankOp a -> Bank -> (a,Bank)
runBankOp (BankOp f) bank = f bank

(+>>) :: BankOp a -> BankOp b -> BankOp b
op1 +>> op2 = BankOp combined
  where combined bank = let (_,bank1) = runBankOp op1 bank
                        in runBankOp op2 bank1

(+>) :: BankOp a -> (a -> BankOp b) -> BankOp b
op +> parameterized = BankOp combined
  where combined bank = let (a,bank1) = runBankOp op bank
                        in runBankOp (parameterized a) bank1

depositOp :: String -> Int -> BankOp ()
depositOp accountName amount = BankOp depositHelper
  where depositHelper bank = ((), deposit accountName amount bank)

withdrawOp :: String -> Int -> BankOp Int
withdrawOp accountName amount = BankOp (withdraw accountName amount)

distributeOp :: String -> String -> Int -> BankOp ()
distributeOp to1 to2 amount = 
  depositOp to1 half
  +>>
  depositOp to2 rest
  where half = div amount 2
        rest = amount - half

shareOp :: String -> String -> String -> BankOp ()
shareOp from to1 to2 = 
  withdrawOp from 100
  +> 
  distributeOp to1 to2

login2 :: String -> Maybe String
login2 "f4bulous!" = Just "unicorn73"
login2 "swordfish" = Just "megahacker"
login2 _           = Nothing

secret :: String -> Maybe String
secret "megahacker" = Just "I like roses"
secret _            = Nothing

stealSecret :: String -> Maybe String
stealSecret password = 
    login2 password >>= 
    secret >>=
    decorate
  where decorate s = return ("Stole secret: " ++ s)
  
increase2 :: Eq a => a -> Int -> [(a,Int)] -> Maybe [(a,Int)]
increase2 key val assocs = 
    lookup key assocs >>= 
    check >>= 
    buildResult 
  where check x 
           | val < x = Nothing
           | otherwise = return x
        buildResult x = return ((key,val) : delete (key,x) assocs)
		
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs


safeNth :: Int -> [a] -> Maybe a
safeNth 0 xs = safeHead xs
safeNth n xs = do t <- safeTail xs
                  safeNth (n-1) t

increase3 :: Eq a => a -> Int -> [(a,Int)] -> Maybe [(a,Int)]
increase3 key val assocs = 
  do oldVal <- lookup key assocs
     check oldVal
     return ((key,val) : delete (key,oldVal) assocs)
  where check x
          | val < x = Nothing
          | otherwise = return x
		 
data State s a = State (s -> (a,s))

runState (State f) s = f s

put :: s -> State s ()
put state = State (\oldState -> ((), state))

get :: State s s
get = State (\state -> (state,state))

modify :: (s -> s) -> State s ()
modify f = State (\state -> ((), f state))

instance Functor State where
  fmap f (State s) = State (f x)
  
instance Applicative State where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = State (\s -> (x,s))
  
  op >>= f = State h
    where h state0 = let (val,state1) = runState op state0
                         op2 = f val
                     in runState op2 state1
					 

					 
-- class Monad m where
  -- (>>=) :: m a -> (a -> m b) -> m b
  
-- instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  
-- instance Monad (State s) where
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
					 
myadd :: Int -> State Int ()
myadd i = do old <- get
             put (old+i)

example :: State Int Int
example = do myadd 3
             value <- get
             myadd 1000
             put (value + 1)
             return value

main = do
  let bank = Bank (Map.fromList [("edsger",10), ("grace",50)])
  
  print(runBankOp (depositOp "edsger" 1) bank)
  print(runBankOp (depositOp "edsger" 1 +>> depositOp "grace" 1) bank)
  print(runBankOp (depositOp "edsger" 1 +>> depositOp "grace" 1 +>> withdrawOp "edsger" 11) bank)
  print(runBankOp (withdrawOp "edsger" 5 +> depositOp "grace") bank)
  print(runBankOp (withdrawOp "edsger" 100 +> depositOp "grace") bank)
  
  print(runBankOp (shareOp "wotan" "sieg" "brun")(Bank (Map.fromList [("brun", 0), ("sieg",0), ("wotan",1000)])))
  print(runBankOp (shareOp "wotan" "sieg" "brun")(Bank (Map.fromList [("brun", 0), ("sieg",0), ("wotan",91	)])))
  
  print("Monads:")
  print(increase3 2 10 [(1,1), (2,2), (3,3)])
  print(increase3 1 0 [(1,1), (2,2), (3,3)])
  
  --print(runState (myadd 1 >> myadd 3 >> myadd 5 >> myadd 6) 0)
  