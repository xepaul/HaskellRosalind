module Rosalind.Server.App where
import Rosalind.Server.Api (app)

import Network.Wai.Handler.Warp ( run, Port )

runServer :: Port ->IO ()
runServer p = run p app