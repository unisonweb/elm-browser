module Main where

import Control.Exception (throwIO)
import Control.Monad
import Data.ByteString (ByteString)
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

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString.Char8
import qualified Data.Text as Text

import Cache


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

      cache :: Cache <-
        newCache

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
        (cors (\_ -> Just simpleCorsResourcePolicy) (app cache))
        `catchIOError` handler


printUsage :: IO ()
printUsage = do
  putStrLn "Usage: unison-browser [PORT]"

app
  :: Cache
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
app cache request respond = do
  ByteString.Char8.putStrLn
    (requestMethod request <> " " <> rawPathInfo request)

  case request of

    GET ["api", "branch", branch] ->
      handleGetBranch cache branch >>= respond


    GET ["api", "declaration", declaration] ->
      handleGetDeclaration cache declaration >>= respond

    GET ["api", "head"] -> do
      [head] <- listDirectory ".unison/v1/paths/_head"
      respond
        (responseLBS
          status200
          [(hContentType, "application/json")]
          (LazyByteString.Char8.pack (show head)))

    GET ["api", "patch", patch] ->
      respond
        (responseFile
          status200
          [(hContentType, "application/octet-stream")]
          (".unison/v1/patches/" <> Text.unpack patch <> ".up")
          Nothing)

    GET ["api", "term", term, "term"] ->
      handleGetTerm cache term >>= respond

    GET ["api", "term", term, "type"] ->
      handleGetTermType cache term >>= respond

    GET ["main.js"] -> do
      mainJs <- getDataFileName "main.js"
      respond
        (responseFile
          status200
          [(hContentType, "text/javascript")]
          mainJs
          Nothing)
    _ -> do
      indexHtml <- getDataFileName "index.html"
      respond
        (responseFile
          status200
          [(hContentType, "text/html")]
          indexHtml
          Nothing)

handleGetBranch
  :: Cache
  -> Text
  -> IO Response
handleGetBranch cache branch =
  handleGetCachedThing
    cache
    (KeyBranch branch)
    (".unison/v1/paths/" <> Text.unpack branch <> ".ub")

handleGetDeclaration
  :: Cache
  -> Text
  -> IO Response
handleGetDeclaration cache declaration =
  handleGetCachedThing
    cache
    (KeyDeclaration declaration)
    (".unison/v1/types/#" <> Text.unpack declaration <> "/compiled.ub")

handleGetTerm
  :: Cache
  -> Text
  -> IO Response
handleGetTerm cache term =
  handleGetCachedThing
    cache
    (KeyTerm term)
    (".unison/v1/terms/#" <> Text.unpack term <> "/compiled.ub")

handleGetTermType
  :: Cache
  -> Text
  -> IO Response
handleGetTermType cache term =
  handleGetCachedThing
    cache
    (KeyTermType term)
    (".unison/v1/terms/#" <> Text.unpack term <> "/type.ub")


handleGetCachedThing
  :: Cache
  -> Key
  -> FilePath
  -> IO Response
handleGetCachedThing cache key path =
  readCache cache key >>= \case
    Nothing -> do
      -- TODO(elliott) we should 404 on file-not-found, not 500
      bytes <- ByteString.readFile path
      writeCache cache key bytes
      pure (makeResponse bytes)

    Just bytes ->
      pure (makeResponse bytes)

  where
    makeResponse :: ByteString -> Response
    makeResponse bytes =
      responseLBS
        status200
        [hContentTypeOctetStream]
        (LazyByteString.fromStrict bytes)

hContentTypeOctetStream :: Header
hContentTypeOctetStream =
  (hContentType, "application/octet-stream")

pattern GET :: [Text] -> Request
pattern GET path <- (asGetRequest -> Just path)

asGetRequest :: Request -> Maybe [Text]
asGetRequest request = do
  guard (requestMethod request == methodGet)
  pure (pathInfo request)
