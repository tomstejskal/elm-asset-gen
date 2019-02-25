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
  }

parser :: Opt.Parser Options
parser =
  Options <$> pathArg <*> forceOverwriteSwitch
  where
    pathArg =
      Opt.argument Opt.str (Opt.metavar "PATH")
    forceOverwriteSwitch =
      Opt.switch $
        Opt.long "force-overwrite"
        <> Opt.short 'f'
        <> Opt.help "Force overwrite output files"
