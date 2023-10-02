{-# LANGUAGE DuplicateRecordFields #-}

module Options where

import Prelude

import Options.Applicative
    ( Parser
    , ReadM
    , eitherReader
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , value
    , (<**>)
    )

data Options = Options
    { fifo :: Maybe FilePath
    , justfile :: Maybe FilePath
    }

parseOptions :: IO Options
parseOptions =
    execParser
        $ info (options <**> helper)
        $ fullDesc
            <> progDesc "TUI over a justfile"
            <> header "vtjust - start a justfile TUI"
  where
    options = Options <$> fifoParser <*> justfileParser

justfileParser :: Parser (Maybe FilePath)
justfileParser =
    option fileReader
        $ mconcat
            [ long "justfile"
            , short 'j'
            , metavar "FILE"
            , help "justfile to use as source"
            , value Nothing
            ]

fileReader :: ReadM (Maybe FilePath)
fileReader = eitherReader $ \case
    file -> Right $ Just file

fifoParser :: Parser (Maybe FilePath)
fifoParser =
    option fileReader
        $ mconcat
            [ long "fifo"
            , short 'j'
            , metavar "FILE"
            , help "fifo to use for IPC communication"
            , value $ Just "vtjust.fifo"
            ]