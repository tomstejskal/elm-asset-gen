{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.Lazy.Encoding as Encoding
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden

import Asset (Asset)
import qualified Asset
import qualified Elm
import qualified JS
import Options (Options(..))

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "Assets"
  [ testElm options
  , testJS options
  ]
  where
    options :: Options
    options = Options
      { path = "test" </> "assets"
      , forceOverwrite = False
      , outputPath = Just "test"
      }

testElm :: Options -> TestTree
testElm options@Options{..} =
  Golden.goldenVsString "Assets.elm" ("test" </> "Assets.elm") $ do
    assets <- Asset.load options
    outputPath' <- maybe Dir.getCurrentDirectory pure outputPath
    Dir.withCurrentDirectory outputPath' $
      pure . Encoding.encodeUtf8 . Elm.gen $ assets

testJS :: Options -> TestTree
testJS options@Options{..} =
  Golden.goldenVsString "Assets.js" ("test" </> "Assets.js") $ do
    assets <- Asset.load options
    outputPath' <- maybe Dir.getCurrentDirectory pure outputPath
    Dir.withCurrentDirectory outputPath' $
      pure . Encoding.encodeUtf8 . JS.gen $ assets
