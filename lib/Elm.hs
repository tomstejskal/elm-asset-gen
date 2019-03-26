{-# LANGUAGE OverloadedStrings #-}

module Elm
  ( gen
  ) where

import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as Writer
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text

import Asset (Asset(..))

gen :: Text -> Asset -> Text
gen moduleName asset =
  genModuleHeader moduleName
  <> (Text.unlines . Writer.execWriter $ genType (Just moduleName) asset)
  <> "\n\n"
  <> genDecoder moduleName asset

genModuleHeader :: Text -> Text
genModuleHeader moduleName =
  Text.unlines 
    [ "module " <> moduleName <> " exposing (" <> moduleName <> ", decoder)"
    , "" 
    , "import Json.Decode as Decode"
    , "import Json.Decode.Extra as Decode"
    ]

genType :: Maybe Text -> Asset -> Writer [Text] Text 
genType name asset = do
  case asset of
    AssetDir n _fp assets -> do
      fields <- traverse (genType Nothing) assets
      let name' = Maybe.fromMaybe n name
      Writer.tell
        [ "\n\ntype alias "
          <> Text.toTitle name'
          <> " =\n"
          <> formatFields fields
          <> indentText <> "}"
        ]
      pure $ n <> " : " <> Text.toTitle name'
    AssetFile n _fp -> do
      let name' = Maybe.fromMaybe n name
      pure $ name' <> " : String"
    where
      formatFields :: [Text] -> Text
      formatFields fields =
        case fields of
          [] -> ""
          (x:xs) ->
            indentText <> "{ " <> x <> "\n"
            <> (Text.unlines $ ((indentText <> ", ") <>) <$> xs)

genDecoder :: Text -> Asset -> Text
genDecoder moduleName asset =
  Text.unlines
    $ "decoder : Decode.Decoder " <> moduleName
    : "decoder ="
    : go "" asset
  where
    go :: Text -> Asset -> [Text]
    go indent asset' =
      case asset' of
        AssetDir name _fp assets ->
          case indent of
            "" ->
              indentText <> "Decode.succeed " <> moduleName : concatMap (go indentText) assets
            _ -> 
              indentText
                <> indent
                <> "|> Decode.andMap\n"
                <> Text.replicate 2 indentText <> indent <> "(Decode.field \""
                <> name
                <> "\"\n"
                <> Text.replicate 3 indentText <> indent <> "(Decode.succeed "
                <> Text.toTitle name
              : concatMap (go $ Text.replicate 3 indentText <> indent) assets
              <>
                [ Text.replicate 3 indentText <> indent <> ")"
                , Text.replicate 2 indentText <> indent <> ")"
                ] 
        AssetFile name _fp ->
          [ indentText
            <> indent
            <> "|> Decode.andMap (Decode.field \""
            <> name
            <> "\" Decode.string)"
          ] 

indentText :: Text
indentText = Text.replicate 4 " "
