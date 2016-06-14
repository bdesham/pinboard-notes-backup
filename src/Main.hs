module Main where

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

logInfo :: String -> IO ()
logInfo = infoM "pnbackup"


-- * Command line parsing

data ProgramOptions = ProgramOptions { o_apiToken :: String
                                     , o_databasePath :: String
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
    where tokenHelp = "Your API token (e.g. maciej:abc123456). "
                      <> "You can find this at <https://pinboard.in/settings/password>."
          pathHelp = "Path to the SQLite database where your notes will be stored. "
                     <> "The default is a file called “Notes.sqlite” in the current directory. "
                     <> "This file will be created if it does not already exist. "
                     <> "Notes are always stored in a table called “notes”."

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
main' (ProgramOptions apiToken databasePath) = do
    conn <- open databasePath
    execute_ conn createTableQuery

    result <- runPinboard apiToken $ do
        notesList <- getNotesList
        for_ notesList $ handleNote conn
    case result of
      Left err -> T.putStrLn err >> exitFailure
      Right _ -> return ()

handleNote :: Connection -> NoteSignature -> PinboardM ()
handleNote conn (NoteSignature noteId lastUpdated) = do
    liftIO $ logInfo $ "Handling note " ++ noteId
    lastUpdatedLocally <- liftIO $ query conn "SELECT updated FROM notes WHERE id=?" (Only noteId)
    if length (lastUpdatedLocally :: [Only UTCTime]) == 0
       then updateNoteFromServer conn noteId
       else if (fromOnly $ head lastUpdatedLocally) < lastUpdated
               then liftIO $ logInfo "This note needs to be updated"
               else liftIO $ logInfo "This note is already up to date"

updateNoteFromServer :: Connection -> String -> PinboardM ()
updateNoteFromServer conn noteId = do
    liftIO $ logInfo ("Downloading note " ++ noteId ++ " from the server")
    note <- getNote noteId
    liftIO $ execute conn insertQuery note
