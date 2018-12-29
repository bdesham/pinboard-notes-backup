module Types ( Note(..)
             , NoteId
             , NoteSignature(..)
             , noteIdToText
             ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField


-- * NoteId

newtype NoteId = NoteId Text
    deriving (Eq, Ord, FromField, FromJSON, ToField)

noteIdToText :: NoteId -> Text
noteIdToText (NoteId t) = t


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

instance FromJSON NoteSignature where
    parseJSON (Object o) = NoteSignature <$>
                           o .: "id" <*>
                           (o .: "updated_at" >>= timeFromString)
        where timeFromString = parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    parseJSON other = typeMismatch "NoteSignature" other
