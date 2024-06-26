-- Welcome to the first exercise set of part 2 of the Haskell Mooc!
-- Edit this file according to the instructions, and check your
-- answers with
--
--   stack runhaskell Set9aTest.hs
--
-- You can also play around with your answers in GHCi with
--
--   stack ghci Set9a.hs

module Set9a where

import Data.Char
import Data.List
import Data.Ord

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Implement a function workload that takes in the number of
-- exercises a student has to finish, and another number that counts
-- the number of hours each exercise takes.
--
-- If the total number of hours needed for all exercises is over 100,
-- return "Holy moly!" if it is under 10, return "Piece of cake!".
-- Otherwise return "Ok."

workload :: Int -> Int -> String
workload nExercises hoursPerExercise = if workload > 100 then "Holy moly!" else if workload < 10 then "Piece of cake!" else "Ok."
  where workload = nExercises * hoursPerExercise

------------------------------------------------------------------------------
-- Ex 2: Implement the function echo that builds a string like this:
--
--   echo "hello!" ==> "hello!, ello!, llo!, lo!, o!, !, "
--   echo "ECHO" ==> "ECHO, CHO, HO, O, "
--   echo "X" ==> "X, "
--   echo "" ==> ""
--
-- Hint: use recursion

echo :: String -> String
echo "" = ""
echo str = str ++ ", " ++ echo (tail str)

------------------------------------------------------------------------------
-- Ex 3: A country issues some banknotes. The banknotes have a serial
-- number that can be used to check if the banknote is valid. For a
-- banknote to be valid, either
--  * the third and fifth digits need to be the same
--  * or the fourth and sixth digits need to be the same
--
-- Given a list of bank note serial numbers (strings), count how many
-- are valid.

isValidBanknote :: String -> Bool
isValidBanknote banknote = banknote !! 2 == banknote !! 4 || banknote !! 3 == banknote !! 5

get_count :: String -> Int
get_count banknote = if isValidBanknote banknote then 1 else 0

countValid :: [String] -> Int
countValid banknotes = countValidHelper 0 banknotes
  where countValidHelper banknotes_so_far [] = banknotes_so_far
        countValidHelper banknotes_so_far (b:bs) = banknotes_so_far + get_count(b) + countValidHelper banknotes_so_far bs

------------------------------------------------------------------------------
-- Ex 4: Find the first element that repeats two or more times _in a
-- row_ in the input list. Return a Nothing value if no element repeats.
--
-- Examples:
--   repeated [1,2,3] ==> Nothing
--   repeated [1,2,2,3,3] ==> Just 2
--   repeated [1,2,1,2,3,3] ==> Just 3

repeated :: Eq a => [a] -> Maybe a
repeated [] = Nothing
repeated [x] = Nothing
repeated (x:xs) = if x == head xs then Just x else repeated xs 

------------------------------------------------------------------------------
-- Ex 5: A laboratory has been collecting measurements. Some of the
-- measurements have failed, so the lab is using the type
--   Either String Int
-- to track the measurements. A Left value represents a failed measurement,
-- while a Right value represents a successful one.
--
-- Compute the sum of all successful measurements. If there are
-- successful measurements, return the sum wrapped in a Right, but if
-- there are none, return Left "no data".
--
-- Examples:
--   sumSuccess [Right 1, Left "it was a snake!", Right 3]
--     ==> Right 4
--   sumSuccess [Left "lab blew up", Left "I was sick"]
--     ==> Left "no data"
--   sumSuccess []
--     ==> Left "no data"

has_no_valid_measurements :: [Either String Int] -> Bool
has_no_valid_measurements [] = True
has_no_valid_measurements ((Right _) : ms) = False
has_no_valid_measurements ((Left _) : ms) = has_no_valid_measurements ms

sumSuccess :: [Either String Int] -> Either String Int
sumSuccess ms = if has_no_valid_measurements ms then Left "no data" else Right (count_successful_sum 0 ms)
  where
    count_successful_sum _ [] = 0
    count_successful_sum sum_so_far (Right x : ms) = x + sum_so_far + count_successful_sum sum_so_far ms
    count_successful_sum sum_so_far (Left _ : ms) = sum_so_far + count_successful_sum sum_so_far ms

------------------------------------------------------------------------------
-- Ex 6: A combination lock can either be open or closed. The lock
-- also remembers a code. A closed lock can only be opened with the
-- right code. The code of an open lock can be changed.
--
-- Implement a datatype Lock and the functions isOpen, open, lock,
-- changeCode and the constant aLock as instructed below.
--
-- Examples:
--   isOpen aLock ==> False
--   isOpen (lock aLock) ==> False
--   isOpen (open "1234" aLock) ==> True
--   isOpen (lock (open "1234" aLock)) ==> False
--   isOpen (open "1235" aLock) ==> False
--   isOpen (lock (open "1235" aLock)) ==> False
--   isOpen (open "1234" (changeCode "0000" aLock)) ==> True
--   isOpen (open "0000" (changeCode "0000" aLock)) ==> False
--   isOpen (open "0000" (lock (changeCode "0000" (open "1234" aLock)))) ==> True
--   isOpen (open "1234" (lock (changeCode "0000" (open "1234" aLock)))) ==> False

data Lock = Locked String | Open String
  deriving (Show)

-- aLock should be a locked lock with the code "1234"
aLock :: Lock
aLock = Locked "1234"

-- isOpen returns True if the lock is open
isOpen :: Lock -> Bool
isOpen (Locked code) = False
isOpen (Open code) = True

-- open tries to open the lock with the given code. If the code is
-- wrong, nothing happens.
open :: String -> Lock -> Lock
open code (Locked actual_code) = if code == actual_code then Open actual_code else Locked actual_code
open code (Open actual_code) = Open actual_code

-- lock closes a lock. If the lock is already closed, nothing happens.
lock :: Lock -> Lock
lock (Locked code) = Locked code
lock (Open code) = Locked code

-- changeCode changes the code of an open lock. If the lock is closed,
-- nothing happens.
changeCode :: String -> Lock -> Lock
changeCode new_code (Locked actual_code) = Locked actual_code
changeCode new_code (Open actual_code) = Open new_code

------------------------------------------------------------------------------
-- Ex 7: Here's a type Text that just wraps a String. Implement an Eq
-- instance for Text that ignores all white space (space characters
-- and line returns).
--
-- Hint: Data.Char.isSpace
--
-- Examples
--   Text "abc"  == Text "abc"      ==> True
--   Text "a bc" == Text "ab  c\n"  ==> True
--   Text "abc"  == Text "abcd"     ==> False
--   Text "a bc" == Text "ab  d\n"  ==> False

data Text = Text String
  deriving (Show)

mycmp :: String -> String -> Bool
mycmp str1 str2 = ignoreSpace str1 == ignoreSpace str2
  where
    processChar ch = if Data.Char.isSpace ch then [] else [ch]
    ignoreSpace [] = ""
    ignoreSpace (ch : str) = processChar ch ++ ignoreSpace str

instance Eq Text where
  (Text "") == (Text "") = True
  (Text succ1) == (Text succ2) = mycmp succ1 succ2


------------------------------------------------------------------------------
-- Ex 8: We can represent functions or mappings as lists of pairs.
-- For example the list [("bob",13),("mary",8)] means that "bob" maps
-- to 13 and "mary" maps to 8.
--
-- Implement _composition_ for mappings like this. You compose two
-- mappings by looking up each result of the first mapping in the
-- second mapping.
--
-- You may assume there are no repeated first elements of tuples in
-- the argument lists, that is.
--
-- The ordering of the output doesn't matter.
--
-- Hint: remember the function `lookup` from Prelude?
--
-- Note! The order of arguments to `compose` is the other way around
-- compared to e.g. (.): `compose f g` should apply `f` first, then
-- `g`, but `f.g` applies `g` first, then `f`.
--
-- Examples:
--   composing two mappings of size 1:
--     compose [("a",1)] [(1,True)]
--       ==> [("a",True)]
--   nonmatching mappings get ignored:
--     compose [("a",1),("b",2)] [(3,False),(4,True)]
--       ==> []
--   a more complex example: note how "omicron" and "c" are ignored
--     compose [("a","alpha"),("b","beta"),("c","gamma")] [("alpha",1),("beta",2),("omicron",15)]
--       ==> [("a",1),("b",2)]

unwrapMaybeJustValue :: Maybe a -> a
unwrapMaybeJustValue (Just x) = x
unwrapMaybeJustValue Nothing = error ("Cannot unwrap Nothing in a Maybe!")

goCompose :: (Eq a, Eq b, Eq c) => (a, b) -> [(b, c)] -> [(a, c)]
goCompose (f1, f2) ss =
  let m = lookup f2 ss
  in if m == Nothing
     then []
     else [(f1, unwrapMaybeJustValue m)]

compose :: (Eq a, Eq b, Eq c) => [(a, b)] -> [(b, c)] -> [(a, c)]
compose [] _ = []
compose _ [] = []
compose (f : fs) ss = goCompose f ss ++ compose fs ss

------------------------------------------------------------------------------
-- Ex 9: Reorder a list using a list of indices.
--
-- You are given a list of indices (numbers from 0 to n) and an input
-- list (of length n). Each index in the index list tells you where to
-- place the corresponding element from the input list in the output
-- list.
--
-- For example, if the 3rd element of the index list is 7, and the 3rd
-- element of the input list is 'a', the output list should have 'a'
-- at index 7.
--
-- (The index lists discussed in this exercise correspond to permutations in
-- math. In fact, permutations can be multiplied which is a special case of
-- the compose function in the previous exercise. For more information on
-- permutations, see https://en.wikipedia.org/wiki/Permutation)
--
-- Examples:
--   permute [0,1] [True, False] ==> [True, False]
--   permute [1,0] [True, False] ==> [False, True]
--   permute [0,1,2,3] "hask" ==> "hask"
--   permute [2,0,1,3] "hask" ==> "ashk"
--   permute [1,2,3,0] "hask" ==> "khas"
--   permute [2, 1, 0] (permute [2, 1, 0] "foo") ==> "foo"
--   permute [1, 0, 2] (permute [0, 2, 1] [9,3,5]) ==> [5,9,3]
--   permute [0, 2, 1] (permute [1, 0, 2] [9,3,5]) ==> [3,5,9]
--   permute ([1, 0, 2] `multiply` [0, 2, 1]) [9,3,5] ==> [5,9,3]
--   permute ([0, 2, 1] `multiply` [1, 0, 2]) [9,3,5] ==> [3,5,9]

-- A type alias for index lists.
type Permutation = [Int]

-- Permuting a list with the identity permutation should change nothing.
identity :: Int -> Permutation
identity n = [0 .. n - 1]

-- This function shows how permutations can be composed. Do not edit this
-- function.
multiply :: Permutation -> Permutation -> Permutation
multiply p q = map (\i -> p !! (q !! i)) (identity (length p))


------------- array -> index -> new value: returns result list
updateList :: [a] -> Int -> a -> [a]
updateList array index value = take index array ++ [value] ++ drop (index + 1) array

-- Permute helper permutation -> original array -> work array -> current index -> n: returns permuted array.
permute' :: Permutation -> [a] -> [a] -> Int -> Int -> [a]
permute' [p] o w i _ = updateList w p (o !! i)
permute' (p : ps) o w i n =
  let nextArray = permute' ps o (updateList w p (o !! i)) (i + 1) n
   in nextArray
permute' p o w i n = if i == n then w else permute' p o w (i + 1) n

-- The actual permutation function:
permute :: Permutation -> [a] -> [a]
permute [] _ = []
permute _ [] = []
permute p o = permute' p o o 0 (length o)
