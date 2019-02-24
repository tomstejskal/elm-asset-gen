{-# LANGUAGE OverloadedStrings #-}

module Asset
  ( Asset(..)
  , fromPath
  ) where

import qualified Data.Char as Char
import Data.Semigroup ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified System.Directory as Dir
import System.FilePath (FilePath, (</>))
import qualified System.FilePath as Path

data Asset
  = AssetFile Text FilePath
  | AssetDir Text FilePath [Asset]
  deriving (Show)

fromPath :: FilePath -> IO Asset
fromPath fp = do
  isDir <- Dir.doesDirectoryExist fp
  name <- pathToName fp
  if isDir
    then do
      files <- fmap (fp </>) <$> Dir.listDirectory fp
      AssetDir name fp <$> traverse fromPath files
    else do
      pure $ AssetFile name fp

pathToName :: FilePath -> IO Text
pathToName fp = do
  go <$> Dir.makeAbsolute fp
  where
    go = lowerFirst
      . Text.filter (Char.isAlphaNum)
      . Text.toTitle
      . Text.map (\c -> if Char.isAlphaNum c then c else ' ')
      . Text.pack
      . Path.takeFileName
      . Path.dropTrailingPathSeparator

    lowerFirst text = 
      (Text.toLower . Text.take 1 $ text) <> Text.drop 1 text
