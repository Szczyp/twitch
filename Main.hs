{-# LANGUAGE DeriveAnyClass, DeriveGeneric, GADTs, NamedFieldPuns,
             NoImplicitPrelude, OverloadedStrings #-}

module Main where

import ClassyPrelude
import Control.Lens
import Data.Aeson.Lens
import Data.Either.Utils
import Data.List              (transpose)
import Data.Time
import Data.Yaml
import Network.Wreq
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
  return $ r ^.. responseBody . key "streams" . values
    . to ((,,,,,)
          <$> view (key "channel" . key "name" . _String)
          <*> view (key "created_at" . _String)
          <*> view (key "viewers" . _Integer . toText)
          <*> view (key "video_height" . _Integer . toText)
          <*> view (key "average_fps" . _Number . to truncate . toText)
          <*> view (key "channel" . key "status" . _String))
  where
    query = "?stream_type=live&limit=100&channel=" ++ intercalate "," channels
    toText = to $ pack . show

printInfo streams = do
  now <- getCurrentTime
  streams
    & each . _2 %~ duration now
    & each . _6 %~ trimStatus
    & each %~ (\(n, c, v, height, fps, s) -> (n, c, v, height ++ "@" ++ fps, s))
    & (("CHANNEL:", "DURATION:", "VIEWERS:", "VIDEO:", "STATUS:") :)
    & map (^.. each . to (text . unpack))
    & transpose
    & map (vcat left)
    & hsep 2 left
    & printBox
  where
    trimStatus s = let s' = take 60 s in if length s > 80 then s' ++ "..." else s
    duration base time = fromMaybe "" $ do
      t <- parse . unpack $ time
      return . pack . format . quotRem (truncate $ diffUTCTime base t / 60) $ 60
      where parse = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
            format (h, m) = show h ++ "h" ++ " " ++ show m ++ "m"

main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  getAppUserDataDirectory "twitch"
    <&> (</> "twitch.config")
    >>= decodeFileEither
    >>= getStreams . forceEither
    >>= printInfo
