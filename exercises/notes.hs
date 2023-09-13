import Data.Char

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
newtype IgnoreCase = IgnoreCase String

ignorecaseHelper :: String -> String
ignorecaseHelper [] = []
ignorecaseHelper (ch:str) = [Data.Char.toLower ch] ++ ignorecaseHelper str

ignorecase :: String -> IgnoreCase
ignorecase str = IgnoreCase (ignorecaseHelper str)

instance Eq IgnoreCase where
  (==) (IgnoreCase str1) (IgnoreCase str2) = ignorecase str1 == ignorecase str2

main = do
  print(ignorecaseHelper "AbCd")
  print (ignorecase "abC" == ignorecase "ABc") -- ==>  True
--  print (ignorecase "acC" == ignorecase "ABc") -- ==>  False