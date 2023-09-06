func :: Int -> Int
func i = square i + 10
  where square x = x * x

main = do
	print ("yo")
	print (func 3)