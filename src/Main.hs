{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Semigroup ((<>))
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Options.Applicative
import qualified System.Directory as Dir

import qualified Asset
import qualified JS
import qualified Elm
import Options (Options(..))
import qualified Options

main :: IO ()
main = do
  Options{..} <- execParser optInfo
  let moduleName' = maybe "Assets" id moduleName
  let jsFile = Text.unpack moduleName' <> ".js"
  let elmFile = Text.unpack moduleName' <> ".elm"
  anyFileExists <- (||)
    <$> Dir.doesFileExist elmFile 
    <*> Dir.doesFileExist jsFile
  if anyFileExists && not forceOverwrite then
    let
      errorMsg =
        jsFile
        <> " and/or "
        <> elmFile
        <> " exists, use -f flag to overwrite these files"
    in
    handleParseResult . Failure $
      parserFailure defaultPrefs optInfo (ErrorMsg errorMsg) []
  else do
    asset <- Asset.fromPath path
    Text.writeFile elmFile $ Elm.gen moduleName' asset
    Text.writeFile jsFile $ JS.gen asset
  where
    optInfo = info (Options.parser <**> helper)
      ( fullDesc
      <> progDesc "Generates Elm assets"
      <> header "elm-asset-gen - Elm assets generator"
      )
