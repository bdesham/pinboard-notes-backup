module Main where

import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable (forM_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Database.SQLite.Simple
import Network.Wreq hiding (header)
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as S
import Options.Applicative
import Prelude hiding (id)


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


-- * Types

data Note = Note { note_id :: Text
                 , note_title :: Text
                 , note_text :: Text
                 , note_hash :: Text
                 , note_created :: Text
                 , note_updated :: Text
                 }

instance ToRow Note where
    toRow (Note n_id n_title n_text n_hash n_created n_updated) =
        toRow (n_id, n_title, n_text, n_hash, n_created, n_updated)


-- * API interaction

allNotesUrl :: String -> String
allNotesUrl = ("https://api.pinboard.in/v1/notes/list?format=json&auth_token="++)

noteUrl :: String -> String -> String
noteUrl token noteId = "https://api.pinboard.in/v1/notes/" ++ noteId ++ "?format=json&auth_token=" ++ token


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

commandLineOptions :: ParserInfo ProgramOptions
commandLineOptions = info (helper <*> optionsParser) parserInfo
    where parserInfo = fullDesc
                       <> header "pnbackup - Back up the notes you’ve saved to Pinboard"
                       <> footer "Copyright © 2016 Benjamin D. Esham."

main :: IO ()
main = execParser commandLineOptions >>= main'


-- * The business logic

main' :: ProgramOptions -> IO ()
main' (ProgramOptions apiToken databasePath) = do
    conn <- open databasePath
    execute_ conn createTableQuery

    S.withAPISession $ \session -> do

        allNotes <- S.get session $ allNotesUrl apiToken
        let maybeTuples = do     -- This "do" block is within the Maybe monad
                bodyString <- allNotes ^? responseBody
                bodyObject <- decode bodyString :: Maybe Object
                notes <- parseMaybe (\obj -> obj .: "notes") bodyObject
                return $ catMaybes $ map noteSummaryToTuple notes
        case maybeTuples of
          Nothing -> putStrLn "Something went wrong"
          Just tuples -> forM_ tuples $ handleNote apiToken session conn

noteSummaryToTuple :: Object -> Maybe (String, UTCTime)
noteSummaryToTuple val = do
    flip parseMaybe val $ \obj -> do
        noteId <- obj .: "id"
        lastUpdated <- obj .: "updated_at"
        lastUpdatedTime <- parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" lastUpdated
        return (noteId, lastUpdatedTime)

handleNote :: String -> Session -> Connection -> (String, UTCTime) -> IO ()
handleNote apiToken session conn (noteId, lastUpdated) = do
    putStrLn $ "handling note " ++ noteId
    lastUpdatedLocally <- query conn "SELECT updated FROM notes WHERE id=?" (Only noteId)
    if length (lastUpdatedLocally :: [Only UTCTime]) == 0
       then putStrLn "need to download this one" >> updateNoteFromServer apiToken session conn noteId
       else if (fromOnly $ head lastUpdatedLocally) < lastUpdated
               then putStrLn "need to update this one"
               else putStrLn "this one is up to date"

updateNoteFromServer :: String -> Session -> Connection -> String -> IO ()
updateNoteFromServer apiToken session conn noteId = do
    response <- S.get session $ noteUrl apiToken noteId
    let noteObject = do     -- This "do" block is within the Maybe monad
            bodyString <- response ^? responseBody
            bodyObject <- decode bodyString :: Maybe Object
            note <- flip parseMaybe bodyObject $ \obj -> do
                id <- obj .: "id"
                title <- obj .: "title"
                text <- obj .: "text"
                hash <- obj .: "hash"
                created <- obj .: "created_at"
                updated <- obj .: "updated_at"
                return $ Note id title text hash created updated
            return note
    case noteObject of
      Nothing -> putStrLn ("Couldn't retrieve note " ++ noteId) >> error "sorry"
      Just n -> execute conn insertQuery n
