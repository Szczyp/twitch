{-# LANGUAGE DeriveAnyClass, DeriveGeneric, GADTs, NamedFieldPuns,
             NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-name-shadowing #-}

module Main where

import ClassyPrelude
import Control.Lens           hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Text
import Data.Either.Utils
import Data.List              (transpose)
import Data.Time
import Data.Yaml
import Network.Wreq
import Options.Applicative    hiding (header)
import System.Directory
import System.IO              (hSetEncoding, utf8)
import Text.PrettyPrint.Boxes

data Config = Config { apiRoot  :: Text
                     , clientId :: Text
                     , channels :: [Text]
                     } deriving (Generic, FromJSON)

getStreams Config {clientId, apiRoot, channels} = do
  let opts = defaults & header "Client-ID" .~ [encodeUtf8 clientId]
  r <- getWith opts . unpack $ apiRoot ++ query
  now <- getCurrentTime
  return $ r ^.. responseBody . key "data" . values
    . to ((,,,)
          <$> view (key "user_name" . _String . to (filter (/= ' ')))
          <*> view (key "started_at" . _String . to (duration now))
          <*> view (key "viewer_count" . _Integer . to (pack . show))
          <*> view (key "title" . _String))
  where
    query = "?first=100" ++ concatMap ("&user_login=" ++) channels
    duration base time = fromMaybe "" $ do
      t <- parse . unpack $ time
      return . pack . format . quotRem (truncate $ diffUTCTime base t / 60) $ 60
      where parse = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
            format (h, m) = show h ++ "h" ++ " " ++ show m ++ "m"

printJSON =
  putStrLn . toStrict . encodeToLazyText . map stream
  where
    stream = object . zipWith (.=) labels . toListOf each
    labels = [ "channel"
             , "duration"
             , "viewers"
             , "title" ]

printInfo skipHeader =
  printBox
  . hsep 2 left
  . map (vcat left)
  . transpose
  . map (^.. each . to (text . unpack))
  . (if skipHeader then id else (("CHANNEL:", "DURATION:", "VIEWERS:", "TITLE:") :))

main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  (json, skipHeader) <- execParser $ info (options <**> helper)
                        (progDesc "Print live twitch streams")
  getAppUserDataDirectory "twitch"
    <&> (</> "twitch.config")
    >>= decodeFileEither
    >>= getStreams . forceEither
    >>= if json then printJSON else printInfo skipHeader
  where
    options = (,)
              <$> switch (long "json" ++ help "print as JSON")
              <*> switch (long "skip-header" ++ help "skip header in default output")
