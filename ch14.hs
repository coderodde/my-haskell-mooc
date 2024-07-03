import qualified Data.Text as T

countLetter :: Char -> T.Text -> Int
countLetter c t = 
    case T.uncons t of
        Nothing -> 0
        Just (x, rest) -> (if x == c then 1 else 0) + countLetter c rest

main = do 
       print(countLetter 't' (T.pack "hello, rottet"))