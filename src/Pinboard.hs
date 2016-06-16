module Pinboard (PinboardM, runPinboard, getNote, getNotesList) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.), (^?))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.State.Lazy (StateT, evalStateT)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format
import Network.Wreq hiding (get, header, put)
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as S (get, withAPISession)
import Types

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds = floor . ((1000000 :: Double) *) . realToFrac

-- | The time we should wait between requests to the API. This is measured in seconds.
delayTime :: NominalDiffTime
delayTime = 3

allNotesUrl :: String
allNotesUrl = "https://api.pinboard.in/v1/notes/list?format=json"

noteUrl :: Text -> String
noteUrl noteId = "https://api.pinboard.in/v1/notes/" ++ noteIdString ++ "?format=json"
    where noteIdString = T.unpack noteId

returnOrThrow :: Maybe a -> Text -> PinboardM a
returnOrThrow Nothing err = throwError err
returnOrThrow (Just value) _ = return value

data PinboardConfig = PinboardConfig { c_token :: String
                                     , c_session :: Session
                                     }

data PinboardState = PinboardState { s_lastSuccess :: UTCTime
                                   }

newtype PinboardM a = Thing {
    runPinboardM :: ReaderT PinboardConfig (StateT PinboardState (ExceptT Text IO)) a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader PinboardConfig,
            MonadState PinboardState, MonadError Text)

runPinboard :: String -> PinboardM a -> IO (Either Text a)
runPinboard token k = S.withAPISession $ \sess -> do
    let config = PinboardConfig token sess
        initialState = PinboardState (posixSecondsToUTCTime 0)
    runExceptT (evalStateT (runReaderT (runPinboardM k) config) initialState)

-- | Takes a URL, appends the API token, makes a GET request, and returns the
-- body of the response (if the status code was 2xx and Wreq gives us a
-- response body) or else Nothing.
performRequest :: String -> PinboardM ByteString
performRequest url = do
    previousTime <- s_lastSuccess <$> get
    session <- c_session <$> ask
    token <- c_token <$> ask

    let urlWithToken = url ++ "&auth_token=" ++ token

    currentTime <- liftIO $ getCurrentTime
    let timeToWait = delayTime - (diffUTCTime currentTime previousTime)
    liftIO $ when (timeToWait > 0) $
        threadDelay (nominalDiffTimeToMicroseconds timeToWait)

    response <- liftIO $ S.get session urlWithToken

    newCurrentTime <- liftIO $ getCurrentTime
    put $ PinboardState newCurrentTime

    let status = response ^. responseStatus ^. statusCode
    if status >= 200 && status < 300
       then let body = response ^? responseBody
             in case body of
                  Nothing -> throwError "The response from the Pinboard API is missing a body"
                  Just b -> return b
       else throwError "Error communicating with the Pinboard API"

noteSummaryToTuple :: Object -> Maybe NoteSignature
noteSummaryToTuple val = do
    flip parseMaybe val $ \obj -> do
        noteId <- obj .: "id"
        lastUpdated <- obj .: "updated_at"
        lastUpdatedTime <- parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" lastUpdated
        return $ NoteSignature noteId lastUpdatedTime

getNote :: Text -> PinboardM Note
getNote noteId = do
    bodyString <- performRequest $ noteUrl noteId
    let noteObject = do     -- This "do" block is within the Maybe monad
            bodyObject <- decode bodyString :: Maybe Object
            note <- flip parseMaybe bodyObject $ \obj -> do
                nid <- obj .: "id"
                title <- obj .: "title"
                text <- obj .: "text"
                hash <- obj .: "hash"
                created <- obj .: "created_at"
                updated <- obj .: "updated_at"
                return $ Note nid title text hash created updated
            return note
    returnOrThrow noteObject ("Couldn't retrieve note " <> noteId)

getNotesList :: PinboardM [NoteSignature]
getNotesList = do
    bodyString <- performRequest allNotesUrl
    let maybeTuples = do     -- This "do" block is within the Maybe monad
            bodyObject <- decode bodyString :: Maybe Object
            notes <- parseMaybe (\obj -> obj .: "notes") bodyObject
            return $ catMaybes $ map noteSummaryToTuple notes
    returnOrThrow maybeTuples "Error getting the list of notes from the server"
