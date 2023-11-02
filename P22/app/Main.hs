module Main (main) where

import Data.List (sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T

nameScore :: T.Text -> Int
nameScore = T.foldl' go 0
  where
    go acc c = acc + fromEnum c - 64

main :: IO ()
main = do
  sortedNames <- sort . fmap (T.dropEnd 1 . T.drop 1) . T.splitOn "," <$> T.readFile "0022_names.txt"
  let result = sum $ zipWith (*) [1 ..] (map nameScore sortedNames)
  print result
