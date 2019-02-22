{-# LANGUAGE OverloadedStrings #-}

module JS
  ( gen
  ) where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (FilePath)

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
          [ "import " <> name <> " from \"" <> Text.pack fp <> "\";" ]

genExport :: Asset -> Text
genExport =
  Text.unlines . go "" True
  where
    go :: Text -> Bool -> Asset -> [Text]
    go indent isLast asset =
      case asset of
        AssetDir name _fp assets ->
          let
            ( hd, term ) =
              case indent of
                "" -> ( "export default {", ";" )
                _ -> ( indent <> name <> ": {", if isLast then "" else "," )
          in
          hd
          : concatMap (go (indent <> "  ") False) (init assets)
          <> (go (indent <> "  ") True $ last assets)
          <> [ indent <> "}" <> term ]
        AssetFile name _fp ->
          [ indent <> name <> ": " <> name <> if isLast then "" else "," ]
        
