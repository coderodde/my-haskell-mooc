module Set10a where

import Data.Char
import Data.List

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Given a list, produce a new list where each element of the
-- original list repeats twice.
--
-- Make sure your function works with infinite lists.
--
-- Examples:
--   doublify [7,1,6]          ==>  [7,7,1,1,6,6]
--   take 10 (doublify [0..])  ==>  [0,0,1,1,2,2,3,3,4,4]

doublify :: [a] -> [a]
doublify [] = []
doublify (x:xs) = [x,x] ++ doublify xs

------------------------------------------------------------------------------
-- Ex 2: Implement the function interleave that takes two lists and
-- produces a new list that takes elements alternatingly from both
-- lists like this:
--
--   interleave [1,2,3] [4,5,6] ==> [1,4,2,5,3,6]
--
-- If one list runs out of elements before the other, just keep adding
-- elements from the other list.
--
-- Make sure your function also works with infinite lists.
--
-- Examples:
--   interleave [1,2,3] [4,5,6]            ==> [1,4,2,5,3,6]
--   interleave [1,2] [4,5,6,7]            ==> [1,4,2,5,6,7]
--   take 10 (interleave [7,7,7] [1..])    ==> [7,1,7,2,7,3,4,5,6,7]
--   take 10 (interleave [1..] (repeat 0)) ==> [1,0,2,0,3,0,4,0,5,0]

interleave :: [a] -> [a] -> [a]
interleave xs ys = interleaveHelper xs ys []
  where interleaveHelper [] ys result = result ++ ys 
        interleaveHelper xs [] result = result ++ xs
        interleaveHelper (x:xs) (y:ys) result = result ++ [x,y] ++ interleaveHelper xs ys result

------------------------------------------------------------------------------
-- Ex 3: Deal out cards. Given a list of players (strings), and a list
-- of cards (strings), deal out the cards to the players in a cycle.
--
-- Make sure your function works with infinite inputs as well!
--
-- Examples:
--   deal ["Hercule","Ariadne"] ["Ace","Joker","Heart"]
--     ==> [("Ace","Hercule"),("Joker","Ariadne"),("Heart","Hercule")]
--   take 4 (deal ["a","b","c"] (map show [0..]))
--     ==> [("0","a"),("1","b"),("2","c"),("3","a")]
--   deal ("you":(repeat "me")) ["1","2","3","4"]
--     ==> [("1","you"),("2","me"),("3","me"),("4","me")]
--
-- Hint: remember the functions cycle and zip?

deal :: [String] -> [String] -> [(String,String)]
deal ps cs = zip cs (cycle ps)

------------------------------------------------------------------------------
-- Ex 4: Compute a running average. Go through a list of Doubles and
-- output a list of averages: the average of the first number, the
-- average of the first two numbers, the first three numbers, and so
-- on.
--
-- Make sure your function works with infinite inputs as well!
--
-- Examples:
--   averages [] ==> []
--   averages [3,2,1] ==> [3.0,2.5,2.0]
--   take 10 (averages [1..]) ==> [1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5]

sumPrefix :: Int -> Int -> [Double] -> Double -> Double
sumPrefix _ _ [] _ = 0.0
sumPrefix _ _ [d] _ = d
sumPrefix index lastIndex (d : ds) acc =
  if index == lastIndex + 1
    then acc
    else d + acc + sumPrefix (index + 1) lastIndex ds acc

avePrefix :: Int -> [Double] -> Double
avePrefix lastIndex ds = sumPrefix 0 lastIndex ds 0.0 / fromIntegral (lastIndex + 1)

temp :: Int -> [Double] -> [Double] -> [Double] -> [Double]
temp _ _ [] resultList = resultList -- Terminate recursion on the end of ds list.
temp currentSize ds (c:cs) resultList = resultList ++ [avePrefix currentSize ds] ++ temp (currentSize + 1) ds cs resultList

averages :: [Double] -> [Double]
averages [] = []
averages ds = temp 0 ds ds []

------------------------------------------------------------------------------
-- Ex 5: Given two lists, xs and ys, and an element z, generate an
-- infinite list that consists of
--
--  * the elements of xs
--  * z
--  * the elements of ys
--  * z
--  * the elements of xs
--  * ... and so on
--
-- Examples:
--   take 20 (alternate "abc" "def" ',') ==> "abc,def,abc,def,abc,"
--   take 10 (alternate [1,2] [3,4,5] 0) ==> [1,2,0,3,4,5,0,1,2,0]

alternate :: [a] -> [a] -> a -> [a]
alternate xs ys z = xs ++ [z] ++ ys ++ [z] ++ alternate xs ys z

------------------------------------------------------------------------------
-- Ex 6: Check if the length of a list is at least n. Make sure your
-- function works for infinite inputs.
--
-- Examples:
--   lengthAtLeast 2 [1,2,3] ==> True
--   lengthAtLeast 7 [1,2,3] ==> False
--   lengthAtLeast 10 [0..]  ==> True

lengthAtLeastHelper :: Int -> Int -> [a] -> Bool
lengthAtLeastHelper tentativeLength minimumLength [] = tentativeLength >= minimumLength
lengthAtLeastHelper tentativeLength minimumLength (x:xs) = 
	if (tentativeLength == minimumLength) 
	then True 
	else lengthAtLeastHelper (tentativeLength + 1) minimumLength xs

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast minimumLength xs = lengthAtLeastHelper 0 minimumLength xs

	------------------------------------------------------------------------------
-- Ex 7: The function chunks should take in a list, and a number n,
-- and return all sublists of length n of the original list. The
-- sublists should be in the order that they appear in the original
-- list. A sublist means a slice, that is, a list of elements
-- a,b,c,... that occur in the original list next to each other and in
-- the same order.
--
-- Make sure your function works with infinite inputs. The function
-- lengthAtLeast can help with this.
--
-- Examples:
--   chunks 2 [1,2,3,4] ==> [[1,2],[2,3],[3,4]]
--   take 4 (chunks 3 [0..]) ==> [[0,1,2],[1,2,3],[2,3,4],[3,4,5]]


lengthAtLeastFrom :: Int -> Int -> [a] -> Bool
lengthAtLeastFrom startIndex size xs = lengthAtLeast (startIndex + size) xs

getSubstring :: Int -> Int -> Int -> [a] -> [a] -> [a] -> [a]
getSubstring _ _ _ _ result [] = result
getSubstring startIndex size processed xs result (w : ws) =
  if not (lengthAtLeastFrom startIndex (size - processed) xs) || processed == size
    then result
    else result ++ (xs !! startIndex) : getSubstring (startIndex + 1) size (processed + 1) xs result ws

-- startIndex -> substringSize -> list -> result:
chunksHelper :: Int -> Int -> [a] -> [[a]] -> [[a]]
chunksHelper startIndex substringSize lst result =
  let ss = getSubstring startIndex substringSize 0 lst [] lst
  in if length ss == 0 
     then result
	 else result ++ [ss] ++ chunksHelper (startIndex + 1) substringSize lst result

chunks :: Int -> [a] -> [[a]]
chunks n xs = chunksHelper 0 n xs []
  
------------------------------------------------------------------------------
-- Ex 8: Define a newtype called IgnoreCase, that wraps a value of
-- type String. Define an `Eq` instance for IgnoreCase so that it
-- compares strings in a case-insensitive way.
--
-- To help the tests, also implement the function
--   ignorecase :: String -> IgnoreCase
--
-- Hint: remember Data.Char.toLower
--
-- Examples:
--   ignorecase "abC" == ignorecase "ABc"  ==>  True
--   ignorecase "acC" == ignorecase "ABc"  ==>  False

ignorecase = todo

------------------------------------------------------------------------------
-- Ex 9: Here's the Room type and some helper functions from the
-- course material. Define a cyclic Room structure like this:
--
--  * maze1 has the description "Maze"
--    * The direction "Left" goes to maze2
--    * "Right" goes to maze3
--  * maze2 has the description "Deeper in the maze"
--    * "Left" goes to maze3
--    * "Right" goes to maze1
--  * maze3 has the description "Elsewhere in the maze"
--    * "Left" goes to maze1
--    * "Right" goes to maze2
--
-- The variable maze should point to the room maze1.
--
-- Examples:
--   play maze ["Left","Left","Left"]
--      ==> ["Maze","Deeper in the maze","Elsewhere in the maze","Maze"]
--   play maze ["Right","Right","Right","Right"]
--      ==> ["Maze","Elsewhere in the maze","Deeper in the maze","Maze","Elsewhere in the maze"]
--   play maze ["Left","Left","Right"]
--      ==> ["Maze","Deeper in the maze","Elsewhere in the maze","Deeper in the maze"]

data Room = Room String [(String,Room)]

-- Do not modify describe, move or play. The tests will use the
-- original definitions of describe, move and play regardless of your
-- modifications.

describe :: Room -> String
describe (Room s _) = s

move :: Room -> String -> Maybe Room
move (Room _ directions) direction = lookup direction directions

play :: Room -> [String] -> [String]
play room [] = [describe room]
play room (d:ds) = case move room d of Nothing -> [describe room]
                                       Just r -> describe room : play r ds

maze :: Room
maze = todo
