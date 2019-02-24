{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.Lazy as Lazy
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden

import Asset (Asset)
import qualified Asset
import qualified Elm
import qualified JS

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "Assets"
  [ testElm
  , testJS
  ]

testElm :: TestTree
testElm = Golden.goldenVsString "Assets.elm" ("test" </> "Assets.elm") $ do
  assets <- Asset.fromPath $ "test" </> "assets"
  pure . Lazy.fromStrict . Encoding.encodeUtf8 . Elm.gen $ assets

testJS :: TestTree
testJS = Golden.goldenVsString "Assets.js" ("test" </> "Assets.js") $ do
  assets <- Asset.fromPath $ "test" </> "assets"
  pure . Lazy.fromStrict . Encoding.encodeUtf8 . JS.gen $ assets
