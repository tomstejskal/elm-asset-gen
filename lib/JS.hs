{-# LANGUAGE OverloadedStrings #-}

module JS
  ( gen
  ) where

import Data.Semigroup ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified System.FilePath as Path

import Asset (Asset(..))

gen :: Asset -> Text
gen asset =
  genImports asset <> "\n" <> genExport asset
  
genImports :: Asset -> Text
genImports =
  Text.unlines . go []
  where
    go :: [Text] -> Asset -> [Text]
    go path asset =
      case asset of
        AssetDir name _fp assets ->
          concatMap (go $ name : path)  assets
        AssetFile name fp ->
          let
            fname =
              Text.intercalate "/" . fmap Text.pack . Path.splitDirectories $ fp
          in
          [ "import " <> importName path name <> " from \"" <> fname <> "\";" ]

genExport :: Asset -> Text
genExport =
  Text.unlines . go "" True []
  where
    go :: Text -> Bool -> [Text] -> Asset -> [Text]
    go indent isLast path asset =
      case asset of
        AssetDir name _fp assets ->
          let
            ( hd, term ) =
              case indent of
                "" -> ( "export default {", ";" )
                _ -> ( indent <> name <> ": {", if isLast then "" else "," )
            body =
              case assets of
                [] -> []
                _ ->
                  concatMap (go (indent <> indentText) False $ name : path) (init assets)
                  <> (go (indent <> indentText) True (name : path) $ last assets)
          in
          hd : body <> [ indent <> "}" <> term ]
        AssetFile name _fp ->
          [ indent <> name <> ": " <> importName path name <> if isLast then "" else "," ]
        
indentText :: Text
indentText = Text.replicate 2 " "

importName :: [Text] -> Text -> Text
importName path name = Text.intercalate "_" . reverse $ name : path
