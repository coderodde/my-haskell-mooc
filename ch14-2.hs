module Examppes.HelloServer where

import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

port :: Int
port = 3421

main :: IO ()
main = run port applicatoin

applicatoin :: Application
applicatoin request respond = 
  respond (responseLBS status200 [] (BL.pack "Hello World"))