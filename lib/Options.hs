{-# LANGUAGE OverloadedStrings #-}

module Options
  ( Options(..)
  , parser
  ) where

import Data.Semigroup ((<>))
import qualified Options.Applicative as Opt
import System.FilePath (FilePath)

data Options = Options
  { path :: FilePath
  , forceOverwrite :: Bool
  , outputPath :: Maybe FilePath
  }

parser :: Opt.Parser Options
parser =
  Options <$> pathArg <*> forceOverwriteSwitch <*> outputPathOpt
  where
    pathArg =
      Opt.argument Opt.str (Opt.metavar "PATH")
    forceOverwriteSwitch =
      Opt.switch $
        Opt.long "force-overwrite"
        <> Opt.short 'f'
        <> Opt.help "Force overwrite output files"
    outputPathOpt =
      Opt.option (Just <$> Opt.str) $
        Opt.long "output-path"
        <> Opt.short 'o'
        <> Opt.metavar "PATH"
        <> Opt.value Nothing
        <> Opt.help "Path of the output files"
