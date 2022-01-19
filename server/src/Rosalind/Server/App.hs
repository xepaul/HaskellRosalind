module Rosalind.Server.App where
import Rosalind.Server.Api (app)

import Network.Wai.Handler.Warp ( run, Port, setLogger, setPort, runSettings, defaultSettings )
import Network.Wai.Logger       (withStdoutLogger)
runServer :: Port ->IO ()
runServer p = do
    withStdoutLogger $ \aplogger -> do
        putStrLn $ "Listening on port " <> show p
        let settings = setPort p $ setLogger aplogger defaultSettings
        runSettings settings app