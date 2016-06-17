module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.List (foldl', intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.IO as T (putStrLn)
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import Data.Version (showVersion)
import Database.SQLite.Simple
import Logging
import Options.Applicative
import Paths_pinboard_notes_backup (version)
import Pinboard
import Prelude hiding (id)
import System.Exit (exitFailure)
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


-- * Reporting

data NoteStatus = New | Updated | UpToDate
    deriving (Eq)

-- | Structure describing how many notes were updated, newly added, deleted, and already up to
-- date, respectively.
data ApplicationResult = ApplicationResult Int Int Int Int

pluralize :: String -> String -> Int -> String
pluralize singular _ 1 = "1 " <> singular
pluralize _ plural n = show n <> " " <> plural

displayResult :: ApplicationResult -> IO ()
displayResult (ApplicationResult upToDate updated new deleted) = do
    logInfo $ intercalate ", " [ updatedString
                               , newString
                               , deletedString
                               , upToDateString
                               ] <> "."
    where upToDateString = pluralize "note already up-to-date" "notes already up-to-date" upToDate
          updatedString = pluralize "note updated" "notes updated" updated
          newString = pluralize "new note" "new notes" new
          deletedString = pluralize "note deleted" "notes deleted" deleted


-- * Command line parsing

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
    setApplicationVerbosity verbosity

    conn <- open databasePath
    execute_ conn createTableQuery

    result <- runPinboard apiToken $ backUpNotes conn
    case result of
      Left err -> T.putStrLn err >> exitFailure
      Right result' -> displayResult result'

backUpNotes :: Connection -> PinboardM ApplicationResult
backUpNotes conn = do
    liftIO $ logInfo "Downloading the list of notes..."
    notesList <- getNotesList

    liftIO $ logInfo "Processing notes (this may take a while)..."
    localNoteOnlyIds <- liftIO $ query_ conn "SELECT id FROM notes"
    let localNoteIds = (Set.fromList . map fromOnly) localNoteOnlyIds
        remoteNoteIds = noteIds notesList
        notesToDelete = localNoteIds `Set.difference` remoteNoteIds
        numberToDelete = length notesToDelete
    liftIO $ for_ notesToDelete $ deleteNote conn
    statuses <- for notesList $ handleNote conn

    return $ ApplicationResult (count UpToDate statuses)
                               (count Updated statuses)
                               (count New statuses)
                               numberToDelete

noteIds :: [NoteSignature] -> Set NoteId
noteIds = Set.fromList . map ns_id

deleteNote :: Connection -> NoteId -> IO ()
deleteNote conn noteId = do
    logDebug $ "Deleting note " ++ (noteIdToString noteId)
    execute conn "DELETE FROM notes WHERE id=?" (Only noteId)

handleNote :: Connection -> NoteSignature -> PinboardM NoteStatus
handleNote conn (NoteSignature noteId lastUpdated) = do
    lastUpdatedLocally <- liftIO $ query conn "SELECT updated FROM notes WHERE id=?" (Only noteId)
    if length (lastUpdatedLocally :: [Only UTCTime]) == 0
       then updateNoteFromServer conn noteId >> return New
       else if (fromOnly $ head lastUpdatedLocally) < lastUpdated
               then updateNoteFromServer conn noteId >> return Updated
               else return UpToDate

updateNoteFromServer :: Connection -> NoteId -> PinboardM ()
updateNoteFromServer conn noteId = do
    liftIO $ logDebug $ "Downloading note " ++ (noteIdToString noteId)
    note <- getNote noteId
    liftIO $ execute conn insertQuery note


-- * Utility functions

-- | Counts the number of occurrences of the given value within the given list.
count :: Eq a => a -> [a] -> Int
count needle = foldl' (\accum item -> if item == needle then succ accum else accum) 0
