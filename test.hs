import Data.Functor

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap g (x:xs) = g x : map g xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node val left right) = Node (f val) (mapTree f left) (mapTree f right)

class Mappable m where
  mapThing :: (a -> b) -> m -> m

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

badMap :: (a -> b) -> [a] -> [b]
badMap f [] = []
badMap f (x:y:xs) = f x : badMap f xs
badMap f (x:xs) = f x : badMap f xs

data Pair a = Pair a a
  deriving Show
  
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
  
instance Foldable Pair where
  foldr f initialValue (Pair x y) = f x (f y initialValue)
  
doubleAndCount :: (Functor f, Foldable f) => f Int -> Int
doubleAndCount = sum . fmap (*2)

main :: IO ()
main = do
  print (fmap (+1) (Pair 3 6))
  print (fmap (+1) [3,6])
  print (foldr (*) 1 (Pair 3 6))
  print (foldr (*) 1 [3,6])
  print (length (Pair 3 6))
  print (length [3,6])
  print (minimum (Pair 3 6))
  print (minimum [3,6])
  print (doubleAndCount (Pair 3 6))
  print (doubleAndCount [3,6])
  