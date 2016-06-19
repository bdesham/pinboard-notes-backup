module Types ( Note(..)
             , NoteId
             , NoteSignature(..)
             , noteIdToString
             , noteIdToText
             ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField


-- * NoteId

newtype NoteId = NoteId Text
    deriving (Eq, Ord, FromField, FromJSON, ToField)

noteIdToText :: NoteId -> Text
noteIdToText = coerce

noteIdToString :: NoteId -> String
noteIdToString = T.unpack . noteIdToText


-- * Note

data Note = Note { note_id :: NoteId
                 , note_title :: Text
                 , note_text :: Text
                 , note_hash :: Text
                 , note_created :: Text
                 , note_updated :: Text
                 }

instance ToRow Note where
    toRow (Note n_id n_title n_text n_hash n_created n_updated) =
        toRow (n_id, n_title, n_text, n_hash, n_created, n_updated)

instance FromJSON Note where
    parseJSON (Object o) = Note <$>
                           o .: "id" <*>
                           o .: "title" <*>
                           o .: "text" <*>
                           o .: "hash" <*>
                           o .: "created_at" <*>
                           o .: "updated_at"
    parseJSON other = typeMismatch "Note" other


-- * NoteSignature

data NoteSignature = NoteSignature { ns_id :: NoteId
                                   , ns_updated :: UTCTime
                                   }
