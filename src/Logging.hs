module Logging where

import Data.Text (Text)
import Data.Text (unpack)
import System.Log.Logger


-- * Types

data Verbosity = Verbose | Standard


-- * Constants

loggerName :: String
loggerName = "pnbackup"


-- * Functions

setApplicationVerbosity :: Verbosity -> IO ()
setApplicationVerbosity Verbose = updateGlobalLogger loggerName $ setLevel DEBUG
setApplicationVerbosity Standard = updateGlobalLogger loggerName $ setLevel INFO

logError :: Text -> IO ()
logError = errorM loggerName . unpack

logInfo :: Text -> IO ()
logInfo = infoM loggerName . unpack

logDebug :: Text -> IO ()
logDebug = debugM loggerName . unpack
