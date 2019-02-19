{-# LANGUAGE OverloadedStrings #-}

module Options
  ( Options(..)
  , parser
  ) where

import qualified Options.Applicative as Opt
import System.FilePath (FilePath)

data Options = Options
  { path :: FilePath
  }

parser :: Opt.Parser Options
parser =
  Options <$> pathArg
  where
    pathArg = Opt.argument Opt.str (Opt.metavar "PATH")
