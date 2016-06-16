module Logging where

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

logInfo :: String -> IO ()
logInfo = infoM loggerName

logDebug :: String -> IO ()
logDebug = debugM loggerName
