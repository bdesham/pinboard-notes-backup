module Utils ( count
             , friendlyReqError
             , pluralize
             , putStrLnErr
             ) where

import Data.List (foldl')
import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)
import System.IO (hFlush, stderr, stdout)
import Utils.FriendlyReqError (friendlyReqError)

-- | Counts the number of occurrences of the given value within the given list.
count :: (Eq a, Foldable t) => a -> t a -> Int
count needle = foldl' (\accum item -> if item == needle then succ accum else accum) 0

pluralize :: Text -> Text -> Int -> Text
pluralize singular _ 1 = "1 " <> singular
pluralize _ plural n = (pack . show) n <> " " <> plural

putStrLnErr :: Text -> IO ()
putStrLnErr message = do
    hFlush stdout
    hPutStrLn stderr message
