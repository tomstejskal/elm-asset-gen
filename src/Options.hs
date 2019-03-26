{-# LANGUAGE OverloadedStrings #-}
module Options
  ( Options(..)
  , parser
  ) where

import Data.Semigroup ((<>))
import Data.Text.Lazy (Text)
import Options.Applicative
import System.FilePath (FilePath)

data Options = Options
  { path :: FilePath
  , forceOverwrite :: Bool
  , moduleName :: Maybe Text
  }

parser :: Parser Options
parser =
  Options <$> pathArg <*> forceOverwriteSwitch <*> moduleNameOption
  where
    pathArg =
      argument str (metavar "PATH")
    forceOverwriteSwitch =
      switch $
        long "force-overwrite"
        <> short 'f'
        <> help "Force overwrite output files"
    moduleNameOption =
      fmap Just
        ( strOption $
          long "module-name"
          <> short 'm'
          <> help "Output module name"
        )
      <|> pure Nothing 
