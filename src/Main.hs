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

-- | For clarity.
type Url = String

main :: IO ()
main =
  get releasesUrl >>= asValue
  >>= (findFirstOsxUrl
       >>> maybe (fail "No OS X download found!") (getUrlAndExtractTo "."))

releasesUrl :: Url
releasesUrl = "https://api.github.com/repos/commercialhaskell/stack/releases/latest"

findFirstOsxUrl :: Response Value -> Maybe Url
findFirstOsxUrl = firstOf $
  responseBody
  . key "assets" . _Array . traverse
  . key "browser_download_url" . _String
  . filtered ("osx.tar.gz" `Text.isInfixOf`)
  . to Text.unpack

getUrlAndExtractTo :: FilePath -> Url -> IO ()
getUrlAndExtractTo destDir tarGzUrl =
  get tarGzUrl
  >>= (view responseBody
       >>> GZip.decompress
       >>> Tar.read
       >>> Tar.unpack destDir)
