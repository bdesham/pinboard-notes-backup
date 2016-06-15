module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import qualified Data.Text.IO as T (putStrLn)
import Data.Time.Clock (UTCTime)
import Data.Version (showVersion)
import Database.SQLite.Simple
import Options.Applicative
import Paths_pinboard_notes_backup (version)
import Pinboard
import Prelude hiding (id)
import System.Exit (exitFailure)
import System.Log.Logger
import Types

-- * Constants

createTableQuery :: Query
createTableQuery = mconcat [ "CREATE TABLE IF NOT EXISTS notes "
                           , "(id TEXT NOT NULL UNIQUE, "
                           , "title TEXT NOT NULL, "
                           , "text TEXT NOT NULL, "
                           , "hash TEXT NOT NULL, "
                           , "created DATETIME NOT NULL, "
                           , "updated DATETIME NOT NULL)"
                           ]

insertQuery :: Query
insertQuery = "INSERT INTO notes (id, title, text, hash, created, updated) VALUES(?, ?, ?, ?, ?, ?)"


-- * Miscellaneous

loggerName :: String
loggerName = "pnbackup"

logInfo :: String -> IO ()
logInfo = infoM loggerName

logDebug :: String -> IO ()
logDebug = debugM loggerName


-- * Command line parsing

data Verbosity = Verbose | Standard
    deriving (Eq)

data ProgramOptions = ProgramOptions { o_apiToken :: String
                                     , o_databasePath :: String
                                     , o_verbosity :: Verbosity
                                     }

optionsParser :: Options.Applicative.Parser ProgramOptions
optionsParser = ProgramOptions
    <$> strOption (short 't'
                   <> long "token"
                   <> metavar "TOKEN"
                   <> help tokenHelp)
    <*> argument str (metavar "PATH"
                      <> value "Notes.sqlite"
                      <> help pathHelp)
    <*> flag Standard Verbose (short 'v'
                               <> long "verbose"
                               <> help verboseHelp)
    where tokenHelp = "Your API token (e.g. maciej:abc123456). "
                      <> "You can find this at <https://pinboard.in/settings/password>."
          pathHelp = "Path to the SQLite database where your notes will be stored. "
                     <> "The default is a file called “Notes.sqlite” in the current directory. "
                     <> "This file will be created if it does not already exist. "
                     <> "Notes are always stored in a table called “notes”."
          verboseHelp = "Display tons of progress information."

addVersionOption :: Options.Applicative.Parser (a -> a)
addVersionOption = infoOption ("pnbackup " <> showVersion version) (long "version")

commandLineOptions :: ParserInfo ProgramOptions
commandLineOptions = info (addVersionOption <*> (helper <*> optionsParser)) parserInfo
    where parserInfo = fullDesc
                       <> header "pnbackup - Back up the notes you’ve saved to Pinboard"
                       <> footer "Copyright © 2016 Benjamin D. Esham"

main :: IO ()
main = execParser commandLineOptions >>= main'


-- * The business logic

main' :: ProgramOptions -> IO ()
main' (ProgramOptions apiToken databasePath verbosity) = do
    updateGlobalLogger loggerName $
        setLevel (if verbosity == Verbose then DEBUG else INFO)

    conn <- open databasePath
    execute_ conn createTableQuery

    result <- runPinboard apiToken $ do
        liftIO $ logInfo "Downloading the list of notes..."
        notesList <- getNotesList
        liftIO $ logInfo "Processing notes..."
        for_ notesList $ handleNote conn
    case result of
      Left err -> T.putStrLn err >> exitFailure
      Right _ -> return ()

handleNote :: Connection -> NoteSignature -> PinboardM ()
handleNote conn (NoteSignature noteId lastUpdated) = do
    lastUpdatedLocally <- liftIO $ query conn "SELECT updated FROM notes WHERE id=?" (Only noteId)
    if length (lastUpdatedLocally :: [Only UTCTime]) == 0
       then updateNoteFromServer conn noteId
       else when ((fromOnly $ head lastUpdatedLocally) < lastUpdated) $
                updateNoteFromServer conn noteId

updateNoteFromServer :: Connection -> String -> PinboardM ()
updateNoteFromServer conn noteId = do
    liftIO $ logDebug ("Downloading note " ++ noteId ++ " from the server")
    note <- getNote noteId
    liftIO $ execute conn insertQuery note
