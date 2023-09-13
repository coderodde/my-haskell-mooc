lengthAtLeastHelper :: Int -> Int -> [a] -> Bool
lengthAtLeastHelper tentativeLength minimumLength [] = tentativeLength >= minimumLength
lengthAtLeastHelper tentativeLength minimumLength (x : xs) =
  if (tentativeLength == minimumLength)
    then True
    else lengthAtLeastHelper (tentativeLength + 1) minimumLength xs

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast minimumLength xs = lengthAtLeastHelper 0 minimumLength xs

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

main = do
  print (chunks 2 [1,2,3,4]) -- ==> [[1,2],[2,3],[3,4]]
  print (take 4 (chunks 3 [0..])) -- ==> [[0,1,2],[1,2,3],[2,3,4],[3,4,5]]