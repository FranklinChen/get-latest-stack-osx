-- | Haskell version of a <https://www.reddit.com/r/haskell/comments/3ksdoh/script_to_download_the_latest_stack/ Python script>.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq (get, asValue, Response, responseBody)
import Control.Arrow ((>>>))
import Control.Lens (firstOf, to, view, filtered)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _Array, _String)
import qualified Data.Text as Text
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS
import Data.List (isSuffixOf)

-- | For clarity.
type Url = String

releasesUrl :: Url
releasesUrl = "https://api.github.com/repos/commercialhaskell/stack/releases/latest"

main :: IO ()
main =
  get releasesUrl
  >>= asValue
  >>= handleReleasesJson

handleReleasesJson :: Response Value -> IO ()
handleReleasesJson =
  findFirstOsxUrl
  >>> maybe (fail "No OS X download found!") getUrlAndExtractStack

findFirstOsxUrl :: Response Value -> Maybe Url
findFirstOsxUrl = firstOf $
  responseBody
  . key "assets" . _Array . traverse
  . key "browser_download_url" . _String
  . filtered ("osx.tar.gz" `Text.isInfixOf`)
  . to Text.unpack

getUrlAndExtractStack :: Url -> IO ()
getUrlAndExtractStack tarGzUrl =
  get tarGzUrl
  >>= handleTarGz

handleTarGz :: Response BS.ByteString -> IO ()
handleTarGz =
  view responseBody
  >>> GZip.decompress
  >>> Tar.read
  >>> Tar.foldEntries extractStackToStdout
      (fail "stack not found in tar archive")
      (fail . ("error processing " ++) . show)

extractStackToStdout :: Tar.Entry -> IO () -> IO ()
extractStackToStdout entry rest
  | "/stack" `isSuffixOf` Tar.entryPath entry
  , Tar.NormalFile content _ <- Tar.entryContent entry = BS.putStr content
  | otherwise = rest
