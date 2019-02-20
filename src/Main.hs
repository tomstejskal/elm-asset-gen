{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Semigroup ((<>))
import qualified Data.Text.IO as Text
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import qualified System.Directory as Dir

import qualified Asset
import qualified JS
import qualified Elm
import Options (Options(..))
import qualified Options

main :: IO ()
main = do
  Options{..} <- Opt.execParser optInfo
  anyFileExists <- (||) <$> Dir.doesFileExist elmFile <*> Dir.doesFileExist jsFile
  if anyFileExists && not forceOverwrite then
    let
      errorMsg = jsFile <> " and/or " <> elmFile <> " exists, use -f flag to overwrite these files"
    in
    Opt.handleParseResult . Opt.Failure $
      Opt.parserFailure Opt.defaultPrefs optInfo (Opt.ErrorMsg errorMsg) []
  else do
    asset <- Asset.fromPath path
    Text.writeFile elmFile $ Elm.gen asset
    Text.writeFile jsFile $ JS.gen asset
  where
    optInfo = Opt.info (Options.parser <**> Opt.helper)
      ( Opt.fullDesc
      <> Opt.progDesc "Generates Elm assets"
      <> Opt.header "elm-asset-gen - Elm assets generator"
      )
    jsFile = "Assets.js"
    elmFile = "Assets.elm"
