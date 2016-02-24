{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import ClassyPrelude
import Control.Lens
import Data.Aeson.Lens
import Data.List              (transpose)
import Data.Time
import Network.Wreq
import Text.PrettyPrint.Boxes

getStreams :: Text -> [Text] -> IO [(Text, Text, Text)]
getStreams url channels = do
  r <- get . unpack $ url ++ "?channel=" ++ intercalate "," channels
  return $ r ^.. responseBody . key "streams" . values
    . to ((,,)
          <$> view (key "channel" . key "name" . _String)
          <*> view (key "created_at" . _String)
          <*> view (key "viewers" . _Integer . to (pack . show)))

duration :: UTCTime -> Text -> Text
duration base time = fromMaybe "" $ do
  t <- parse . unpack $ time
  return . pack . format . quotRem (truncate $ diffUTCTime base t / 60) $ 60
  where parse = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
        format (h, m) = show h ++ "h" ++ " " ++ show m ++ "m"

printInfo :: [(Text, Text, Text)] -> IO ()
printInfo =
  printBox
  . hsep 2 left
  . map (vcat left)
  . transpose
  . map (^.. each . to (text . unpack))
  . (("CHANNEL:", "DURATION:", "VIEWERS:") :)

main :: IO ()
main = do
  config <- readFile "config.json" :: IO Text
  now <- getCurrentTime
  streams <- getStreams
    (config ^. key "api-root" . _String)
    (config ^.. key "channels" . values . _String)
  printInfo $ streams & each . _2 %~ duration now
