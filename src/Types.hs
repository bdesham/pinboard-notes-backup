module Types (Note(..), NoteSignature(..)) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple

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

data NoteSignature = NoteSignature { ns_id :: Text
                                   , ns_updated :: UTCTime
                                   }
