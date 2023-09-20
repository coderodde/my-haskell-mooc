produceThree :: IO Int
produceThree = return 3

printThree :: IO ()
printThree = do
  three <- produceThree
  putStrLn (show three)

yesNoQuestion :: String -> IO Bool
yesNoQuestion question = do
  putStrLn question
  s <- getLine
  return (s == "Y")

main = do
  three <- produceThree
  print (three)
  yesNoQuestion "Hello?"