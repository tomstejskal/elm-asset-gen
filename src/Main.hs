{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Semigroup ((<>))
import qualified Data.Text.IO as Text
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt

import qualified Asset
import qualified JS
import qualified Elm
import Options (Options(..))
import qualified Options

main :: IO ()
main = do
  Options{..} <- Opt.execParser opts
  asset <- Asset.fromPath path
  Text.writeFile "Assets.elm" $ Elm.gen asset
  Text.writeFile "Assets.js" $ JS.gen asset
  where
    opts = Opt.info (Options.parser <**> Opt.helper)
      ( Opt.fullDesc
      <> Opt.progDesc "Generates Elm assets"
      <> Opt.header "elm-asset-gen - Elm assets generator"
      )
