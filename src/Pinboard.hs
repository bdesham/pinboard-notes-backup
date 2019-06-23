module Pinboard ( PinboardM
                , runPinboard
                , backUpNotes
                ) where

import Prelude hiding (log, putStrLn)
import Control.Concurrent (threadDelay)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.State.Lazy (StateT, evalStateT)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 as B (pack)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Traversable (for)
import Data.Version (showVersion)
import Database.SQLite.Simple
import Network.HTTP.Req
import Paths_pinboard_notes_backup (version)
import Types
import Utils (count, friendlyReqError)


-- * Constants

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds = floor . ((1000000 :: Double) *) . realToFrac

-- | The time we should wait between requests to the API. This is measured in seconds.
delayTime :: NominalDiffTime
delayTime = 3

allNotesUrl :: Url 'Https
allNotesUrl = https "api.pinboard.in" /: "v1" /: "notes" /: "list"

noteUrl :: NoteId -> Url 'Https
noteUrl noteId = https "api.pinboard.in" /: "v1" /: "notes" /: (noteIdToText noteId)


-- * SQLite queries

selectUpdatedQuery :: Query
selectUpdatedQuery = "SELECT updated FROM notes WHERE id=?"

deleteQuery :: Query
deleteQuery = "DELETE FROM notes WHERE id=?"

insertQuery :: Query
insertQuery = "INSERT INTO notes (id, title, text, hash, created, updated) VALUES(?, ?, ?, ?, ?, ?)"


-- * Types

data NoteStatus = New | Updated | UpToDate
    deriving (Eq)

data PinboardConfig = PinboardConfig { c_token :: String
                                     , c_verbosity :: Verbosity
                                     }

data PinboardState = PinboardState { s_lastSuccess :: UTCTime
                                   }

newtype PinboardM a = Thing {
    runPinboardM :: ReaderT PinboardConfig (StateT PinboardState (ExceptT Text IO)) a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader PinboardConfig,
            MonadState PinboardState, MonadError Text)

instance MonadHttp PinboardM where
    handleHttpException :: HttpException -> PinboardM a
    handleHttpException = throwError . friendlyReqError

runPinboard :: String -> Verbosity -> PinboardM a -> IO (Either Text a)
runPinboard token verbosity k =
    let config = PinboardConfig token verbosity
        initialState = PinboardState (posixSecondsToUTCTime 0)
     in runExceptT (evalStateT (runReaderT (runPinboardM k) config) initialState)


-- * Helper functions

-- | Takes a URL, appends the API token, makes a GET request, and returns the body of the response
-- (if the status code was 2xx) or else Nothing.
performRequest :: Url 'Https -> PinboardM ByteString
performRequest url = do
    previousTime <- s_lastSuccess <$> get
    currentTime <- liftIO $ getCurrentTime
    let timeToWait = delayTime - (diffUTCTime currentTime previousTime)
    liftIO $ when (timeToWait > 0) $
        threadDelay (nominalDiffTimeToMicroseconds timeToWait)

    opts <- reqOptions
    response <- req GET url NoReqBody lbsResponse opts

    newCurrentTime <- liftIO $ getCurrentTime
    put $ PinboardState newCurrentTime

    return $ responseBody response

reqOptions :: PinboardM (Option scheme)
reqOptions = do
    token <- c_token <$> ask
    return $ mconcat [ header "User-Agent" $ B.pack userAgent
                     , "format" =: ("json" :: Text)
                     , "auth_token" =: token
                     ]
    where userAgent = "pnbackup/" <> showVersion version <> " (+" <> url <> ")"
          url = "https://github.com/bdesham/pinboard-notes-backup"

returnOrThrow :: Maybe a -> Text -> PinboardM a
returnOrThrow Nothing err = throwError err
returnOrThrow (Just value) _ = return value


-- * Business logic

backUpNotes :: Connection -> PinboardM ApplicationResult
backUpNotes conn = do
    log "Downloading the list of notes..."
    notesList <- getNotesList

    log "Processing notes (this may take a while)..."
    localNoteOnlyIds <- liftIO $ query_ conn "SELECT id FROM notes"
    let localNoteIds = (Set.fromList . map fromOnly) localNoteOnlyIds
        remoteNoteIds = (Set.fromList . map ns_id) notesList
        notesToDelete = localNoteIds \\ remoteNoteIds
        numberToDelete = length notesToDelete
    for_ notesToDelete $ deleteNote conn
    statuses <- for notesList $ handleNote conn

    return $ ApplicationResult (count UpToDate statuses)
                               (count Updated statuses)
                               (count New statuses)
                               numberToDelete

handleNote :: Connection -> NoteSignature -> PinboardM NoteStatus
handleNote conn (NoteSignature noteId lastUpdated) = do
    lastUpdatedLocally <- liftIO $ query conn selectUpdatedQuery (Only noteId)
    if length (lastUpdatedLocally :: [Only UTCTime]) == 0
       then updateNoteFromServer conn noteId >> return New
       else if (fromOnly $ head lastUpdatedLocally) < lastUpdated
               then updateNoteFromServer conn noteId >> return Updated
               else return UpToDate

getNote :: NoteId -> PinboardM Note
getNote noteId = do
    logVerbose $ "Downloading note " <> (noteIdToText noteId)
    body <- performRequest $ noteUrl noteId
    let noteObject = decode body :: Maybe Note
    returnOrThrow noteObject ("Couldn't retrieve note " <> noteIdToText noteId)

getNotesList :: PinboardM [NoteSignature]
getNotesList = do
    body <- performRequest allNotesUrl
    let noteSignatures = do     -- This "do" block is within the Maybe monad
            bodyObject <- decode body :: Maybe Object
            notes <- (parseMaybe (\obj -> obj .: "notes") bodyObject) :: Maybe [NoteSignature]
            return notes
    returnOrThrow noteSignatures "Error getting the list of notes from the server"

deleteNote :: Connection -> NoteId -> PinboardM ()
deleteNote conn noteId = do
    logVerbose $ "Deleting note " <> (noteIdToText noteId)
    liftIO $ execute conn deleteQuery (Only noteId)

updateNoteFromServer :: Connection -> NoteId -> PinboardM ()
updateNoteFromServer conn noteId = do
    note <- getNote noteId
    liftIO $ execute conn deleteQuery (Only noteId)
    liftIO $ execute conn insertQuery note


-- * Logging

log :: Text -> PinboardM ()
log = liftIO . putStrLn

-- | Prints the given message, but only if the application was run with --verbose.
logVerbose :: Text -> PinboardM ()
logVerbose message = do
    verbosity <- c_verbosity <$> ask
    when (verbosity == Verbose) $
        liftIO $ putStrLn message
