module Utils ( count
             , pluralize
             ) where

import Data.List (foldl')
import Data.Text (Text, pack)

-- | Counts the number of occurrences of the given value within the given list.
count :: (Eq a, Foldable t) => a -> t a -> Int
count needle = foldl' (\accum item -> if item == needle then succ accum else accum) 0

pluralize :: Text -> Text -> Int -> Text
pluralize singular _ 1 = "1 " <> singular
pluralize _ plural n = (pack . show) n <> " " <> plural
