-- currentIndex -> prefixLength -> data -> acc
sumPrefix :: Int -> Int -> [Double] -> Double -> Double
sumPrefix _ _ [] acc = acc
sumPrefix index prefixLength (d : ds) acc =
  if index == prefixLength
    then acc
    else d + acc + sumPrefix (index + 1) prefixLength ds acc

avePrefix :: Int -> [Double] -> Double
avePrefix prefixLength ds = (sumPrefix 0 prefixLength ds 0.0) / fromIntegral prefixLength

-- currentSize -> data -> counter data -> result:
averagesHelper :: Int -> [Double] -> [Double] -> [Double] -> [Double]
averagesHelper _ _ [] resultList = resultList -- Terminate recursion on the end of ds list.
averagesHelper currentSize ds (c:cs) resultList = resultList ++ [avePrefix currentSize ds] ++ averagesHelper (currentSize + 1) ds cs resultList

averages :: [Double] -> [Double]
averages [] = []
averages ds = averagesHelper 1 ds ds []

main = do
  print(sumPrefix 0 1 [3.0, 1.0, 5.0] 0.0)
  print(avePrefix 3 [3.0, 1.0, 5.0])
  print(averages [1.0,2.0,3.0])
  