{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad
import Data.Function ((&))
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Types hiding (StdMethod(..))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy)
import Paths_unison_browser (getDataFileName)
import Prelude hiding (head)
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Text.Read

import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as LazyByteString.Char8
import qualified Data.Text as Text


main :: IO ()
main =
  doesDirectoryExist ".unison" >>= \case
    False -> do
      hPutStrLn stderr ".unison/ not found!"
      exitFailure

    True -> do
      port :: Int <-
        getArgs >>= \case
          ["--help"] -> printUsage >> exitSuccess
          [readMaybe -> Just port] -> pure port
          [] -> pure 8180
          _ -> printUsage >> exitFailure

      let
        handler :: IOError -> IO ()
        handler err =
          case (ioeGetLocation err, ioeGetErrorString err) of
            ("Network.Socket.bind", "resource busy") -> do
              hPutStrLn stderr ("Failed to bind to 127.0.0.1:" ++ show port)
              hPutStrLn stderr "(Do you have unison-browser running in another terminal?)"
              hPutStrLn stderr "Pass a port number on the command line to change the port."
              exitFailure

            _ ->
              throwIO err

      runSettings
        (defaultSettings
          & setBeforeMainLoop
              (putStrLn
                (concat
                  [ "Running on 127.0.0.1:"
                  , show port
                  , ", allowing cross-origin requests"
                  ]))
          & setHost "127.0.0.1"
          & setPort port)
        (cors (\_ -> Just simpleCorsResourcePolicy) app)
        `catchIOError` handler


printUsage :: IO ()
printUsage = do
  putStrLn "Usage: unison-browser [PORT]"

app
  :: Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
app request respond = do
  ByteString.Char8.putStrLn
    (requestMethod request <> " " <> rawPathInfo request)

  case request of
    GET [] -> do
      indexHtml <- getDataFileName "index.html"
      respond
        (responseFile
          status200
          [(hContentType, "text/html")]
          indexHtml
          Nothing)

    GET ["branch", branch] ->
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

    GET ["patch", patch] ->
      respond
        (responseFile
          status200
          [(hContentType, "application/octet-stream")]
          (".unison/v1/patches/" <> Text.unpack patch <> ".up")
          Nothing)

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
