{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import Data.Text (Text)
import Network.HTTP.Types hiding (StdMethod(..))
import Network.Wai
import Network.Wai.Cli
import Paths_unison_browser (getDataFileName)
import Prelude hiding (head)
import System.Directory
import System.Exit
import System.IO

import qualified Data.ByteString.Lazy.Char8 as LazyByteString.Char8
import qualified Data.Text as Text


main :: IO ()
main =
  doesDirectoryExist ".unison" >>= \case
    False -> do
      hPutStrLn stderr ".unison/ not found!"
      exitFailure

    True ->
      defWaiMain app

app
  :: Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
app request respond =
  case request of
    GET [] -> do
      indexHtml <- getDataFileName "index.html"
      respond
        (responseFile
          status200
          [(hContentType, "text/html")]
          indexHtml
          Nothing)

    GET ["branch", branch] -> do
      respond
        (responseFile
          status200
          [(hContentType, "application/octet-stream")]
          (".unison/v1/paths/" <> Text.unpack branch <> ".ub")
          Nothing)

    GET ["declaration", declaration] ->
      respond
        (responseFile
          status200
          [(hContentType, "application/octet-stream")]
          (".unison/v1/types/#" <> Text.unpack declaration <> "/compiled.ub")
          Nothing)

    GET ["head"] -> do
      [head] <- listDirectory ".unison/v1/paths/_head"
      respond
        (responseLBS
          status200
          [(hContentType, "application/json")]
          (LazyByteString.Char8.pack (show head)))

    GET ["term", term, "term"] ->
      respond
        (responseFile
          status200
          [(hContentType, "application/octet-stream")]
          (".unison/v1/terms/#" <> Text.unpack term <> "/compiled.ub")
          Nothing)

    GET ["term", term, "type"] ->
      respond
        (responseFile
          status200
          [(hContentType, "application/octet-stream")]
          (".unison/v1/terms/#" <> Text.unpack term <> "/type.ub")
          Nothing)

    _ ->
      respond (responseLBS status404 [] "")


pattern GET :: [Text] -> Request
pattern GET path <- (asGetRequest -> Just path)

asGetRequest :: Request -> Maybe [Text]
asGetRequest request = do
  guard (requestMethod request == methodGet)
  pure (pathInfo request)
