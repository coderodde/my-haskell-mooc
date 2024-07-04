import qualified Data.Text as T
import Text.Read (readMaybe)

shit1 :: T.Text -> String
shit1 txt = T.unpack txt

data Command = Deposit T.Text Int | Balance T.Text
  deriving (Show, Eq)

parseInt :: T.Text -> Maybe Int
parseInt = readMaybe . T.unpack

extractIntFromMaybe :: Maybe Int -> Int
extractIntFromMaybe (Just i) = i
extractIntFromMaybe Nothing = -1

textBalance :: T.Text
textDeposit :: T.Text

textBalance = T.pack "balance"
textDeposit = T.pack "deposit"

parseCommand :: [T.Text] -> Maybe Command
parseCommand [textBalance, account] = Just (Balance account)
parseCommand [textDeposit, account, amount] = Just (Deposit account (extractIntFromMaybe (parseInt amount)))
parseCommand _ = Nothing

main = do
  print(shit1 (T.pack "abc"))
  print(shit1 (T.pack "cba"))
  print(parseCommand [textBalance, T.pack "simo"])
  print(parseCommand [T.pack "deposit", T.pack "anni", T.pack "13"])
  print(parseCommand [])