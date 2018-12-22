module Pinboard (PinboardM, runPinboard, getNote, getNotesList) where

import Control.Concurrent (threadDelay)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.State.Lazy (StateT, evalStateT)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 as B (pack)
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Version (showVersion)
import Network.HTTP.Req
import Paths_pinboard_notes_backup (version)
import Types

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds = floor . ((1000000 :: Double) *) . realToFrac

-- | The time we should wait between requests to the API. This is measured in seconds.
delayTime :: NominalDiffTime
delayTime = 3

allNotesUrl :: Url 'Https
allNotesUrl = https "api.pinboard.in" /: "v1" /: "notes" /: "list"

noteUrl :: NoteId -> Url 'Https
noteUrl noteId = https "api.pinboard.in" /: "v1" /: "notes" /: (T.pack noteIdString)
    where noteIdString = noteIdToString noteId

returnOrThrow :: Maybe a -> Text -> PinboardM a
returnOrThrow Nothing err = throwError err
returnOrThrow (Just value) _ = return value

data PinboardConfig = PinboardConfig { c_token :: String
                                     }

data PinboardState = PinboardState { s_lastSuccess :: UTCTime
                                   }

newtype PinboardM a = Thing {
    runPinboardM :: ReaderT PinboardConfig (StateT PinboardState (ExceptT Text IO)) a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader PinboardConfig,
            MonadState PinboardState, MonadError Text)

runPinboard :: String -> PinboardM a -> IO (Either Text a)
runPinboard token k =
    let config = PinboardConfig token
        initialState = PinboardState (posixSecondsToUTCTime 0)
     in runExceptT (evalStateT (runReaderT (runPinboardM k) config) initialState)

reqOptions :: PinboardM (Option scheme)
reqOptions = do
    token <- c_token <$> ask
    return $ mconcat [ header "User-Agent" $ B.pack userAgent
                     , "format" =: ("json" :: Text)
                     , "auth_token" =: token
                     ]
    where userAgent = "pnbackup/" <> showVersion version <> " (+" <> url <> ")"
          url = "https://github.com/bdesham/pinboard-notes-backup"

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
    response <- liftIO $ runReq def $ req GET url NoReqBody lbsResponse opts

    newCurrentTime <- liftIO $ getCurrentTime
    put $ PinboardState newCurrentTime

    let status = responseStatusCode response
    if status >= 200 && status < 300
       then return $ responseBody response
       else throwError "Error communicating with the Pinboard API"

getNote :: NoteId -> PinboardM Note
getNote noteId = do
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
