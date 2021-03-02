{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import Prelude hiding (id, putStrLn)
import Data.Monoid ((<>))
import Data.Text (intercalate)
import Data.Text.IO (putStrLn)
import Data.Version (showVersion)
import Database.SQLite.Simple
import Options.Applicative
import Paths_pinboard_notes_backup (version)
import Pinboard
import System.Exit (exitFailure)
import Text.PrettyPrint.ANSI.Leijen (Doc, vsep)
import Types
import Utils (pluralize, putStrLnErr)


-- * Command line parsing

data ProgramOptions = ProgramOptions { o_apiToken :: String
                                     , o_verbosity :: Verbosity
                                     , o_databasePath :: String
                                     }

optionsParser :: Options.Applicative.Parser ProgramOptions
optionsParser = ProgramOptions
    <$> strOption (short 't'
                   <> long "token"
                   <> metavar "TOKEN"
                   <> help tokenHelp)
    <*> flag Standard Verbose (short 'v'
                               <> long "verbose"
                               <> help verboseHelp)
    <*> argument str (metavar "FILE"
                      <> help pathHelp
                      <> action "file")
    where tokenHelp = "Your API token (e.g. maciej:abc123456). "
                      <> "You can find this at <https://pinboard.in/settings/password>."
          verboseHelp = "Display detailed progress information."
          pathHelp = "Filename of the SQLite database where your notes will be stored. "
                     <> "This file will be created if it does not already exist. "
                     <> "Notes are always stored in a table called \"notes\"."

addVersionOption :: Options.Applicative.Parser (a -> a)
addVersionOption = infoOption ("pnbackup " <> showVersion version)
                              (long "version"
                               <> help "Show the version number"
                               <> hidden)

footerText :: Doc
footerText = vsep [ "For more information, see \"man pnbackup\"."
                  , ""
                  , "Copyright © 2016–2017, 2019, 2021 Benjamin D. Esham"
                  , ""
                  , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
                  , "This is free software: you are free to change and redistribute it."
                  , "There is NO WARRANTY, to the extent permitted by law."
                  ]

commandLineOptions :: ParserInfo ProgramOptions
commandLineOptions = info (addVersionOption <*> helper <*> optionsParser) parserInfo
    where parserInfo = fullDesc
                       <> header "pnbackup -- Back up the notes you've saved to Pinboard"
                       <> footerDoc (Just footerText)


-- * Main

main :: IO ()
main = execParser commandLineOptions >>= main'

main' :: ProgramOptions -> IO ()
main' (ProgramOptions apiToken verbosity databasePath) = do
    conn <- open databasePath
    execute_ conn createTableQuery

    result <- runPinboard apiToken verbosity $ backUpNotes conn
    case result of
      Left err -> putStrLnErr ("pnbackup: " <> err) >> exitFailure
      Right result' -> displayResult result'

createTableQuery :: Query
createTableQuery = mconcat [ "CREATE TABLE IF NOT EXISTS notes "
                           , "(id TEXT NOT NULL UNIQUE, "
                           , "title TEXT NOT NULL, "
                           , "text TEXT NOT NULL, "
                           , "hash TEXT NOT NULL, "
                           , "created DATETIME NOT NULL, "
                           , "updated DATETIME NOT NULL)"
                           ]

displayResult :: ApplicationResult -> IO ()
displayResult (ApplicationResult upToDate updated new deleted) = do
    putStrLn $ intercalate ", " [ updatedString
                                , newString
                                , deletedString
                                , upToDateString
                                ] <> "."
    where upToDateString = pluralize "note already up-to-date" "notes already up-to-date" upToDate
          updatedString = pluralize "note updated" "notes updated" updated
          newString = pluralize "new note" "new notes" new
          deletedString = pluralize "note deleted" "notes deleted" deleted
