{-# LANGUAGE OverloadedStrings #-}

module JS
  ( gen
  ) where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (FilePath)
import qualified System.FilePath as Path

import Asset (Asset(..))

gen :: Asset -> Text
gen asset =
  genImports asset <> "\n" <> genExport asset
  
genImports :: Asset -> Text
genImports =
  Text.unlines . go []
  where
    go :: [FilePath] -> Asset -> [Text]
    go path asset =
      case asset of
        AssetDir _name fp assets ->
          concatMap (go $ fp : path) assets
        AssetFile name fp ->
          let
            fname = Path.joinPath . reverse $ fp : path
          in
          [ "import " <> name <> " from \"" <> Text.pack fname <> "\";" ]

genExport :: Asset -> Text
genExport =
  Text.unlines . go ""
  where
    go :: Text -> Asset -> [Text]
    go indent asset =
      case asset of
        AssetDir name _fp assets ->
          let
            ( hd, term ) =
              case indent of
                "" -> ( "export default {", ";" )
                _ -> ( indent <> name <> ": {", "," )
          in
          hd : concatMap (go $ indent <> "  ") assets <> [ indent <> "}" <> term ]
        AssetFile name _fp ->
          [ indent <> name <> ": " <> name <> "," ]
        
