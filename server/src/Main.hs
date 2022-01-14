module Main where
import Api (app)
import Servant
import Servant.Server

import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 app
