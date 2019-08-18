-- | Better http library.
-- TODO reduce duplication in this module


module Ucb.Util.Http exposing
    ( Error(..)
    , Response
    , getBytes
    , getJson
    , getRawBytes
    )

import Bytes exposing (Bytes)
import Bytes.Decode
import Dict exposing (Dict)
import Http
import Json.Decode
import Task exposing (Task)


type Error a
    = BadUrl String
    | Timeout
    | NetworkError
    | BadBody (Response a) String
    | BadStatus (Response a)


type alias Response a =
    { url : String
    , statusCode : Int
    , statusText : String
    , headers : Dict String String
    , body : a
    }


{-| GET a Json blob.
-}
getJson :
    { decoder : Json.Decode.Decoder a
    , headers : List ( String, String )
    , timeout : Maybe Float
    , url : String
    }
    -> Task (Error String) (Response a)
getJson { decoder, headers, timeout, url } =
    Http.task
        { body = Http.emptyBody
        , headers = List.map (\( k, v ) -> Http.header k v) headers
        , method = "GET"
        , resolver = jsonResolver decoder
        , timeout = timeout
        , url = url
        }


{-| GET bytes.
-}
getBytes :
    { decoder : Bytes.Decode.Decoder a
    , headers : List ( String, String )
    , timeout : Maybe Float
    , url : String
    }
    -> Task (Error Bytes) (Response a)
getBytes { decoder, headers, timeout, url } =
    Http.task
        { body = Http.emptyBody
        , headers = List.map (\( k, v ) -> Http.header k v) headers
        , method = "GET"
        , resolver = bytesResolver decoder
        , timeout = timeout
        , url = url
        }


{-| GET raw bytes.
-}
getRawBytes :
    { headers : List ( String, String )
    , timeout : Maybe Float
    , url : String
    }
    -> Task (Error Bytes) (Response Bytes)
getRawBytes { headers, timeout, url } =
    Http.task
        { body = Http.emptyBody
        , headers = List.map (\( k, v ) -> Http.header k v) headers
        , method = "GET"
        , resolver = rawBytesResolver
        , timeout = timeout
        , url = url
        }


jsonResolver :
    Json.Decode.Decoder a
    -> Http.Resolver (Error String) (Response a)
jsonResolver =
    jsonParser >> Http.stringResolver


jsonParser :
    Json.Decode.Decoder a
    -> Http.Response String
    -> Result (Error String) (Response a)
jsonParser decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err <|
                BadStatus
                    { url = metadata.url
                    , statusCode = metadata.statusCode
                    , statusText = metadata.statusText
                    , headers = metadata.headers
                    , body = body
                    }

        Http.GoodStatus_ metadata body ->
            case Json.Decode.decodeString decoder body of
                Err err ->
                    Err <|
                        BadBody
                            { url = metadata.url
                            , statusCode = metadata.statusCode
                            , statusText = metadata.statusText
                            , headers = metadata.headers
                            , body = body
                            }
                            (Json.Decode.errorToString err)

                Ok contents ->
                    Ok
                        { url = metadata.url
                        , statusCode = metadata.statusCode
                        , statusText = metadata.statusText
                        , headers = metadata.headers
                        , body = contents
                        }


bytesResolver :
    Bytes.Decode.Decoder a
    -> Http.Resolver (Error Bytes) (Response a)
bytesResolver =
    bytesParser >> Http.bytesResolver


bytesParser :
    Bytes.Decode.Decoder a
    -> Http.Response Bytes
    -> Result (Error Bytes) (Response a)
bytesParser decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err <|
                BadStatus
                    { url = metadata.url
                    , statusCode = metadata.statusCode
                    , statusText = metadata.statusText
                    , headers = metadata.headers
                    , body = body
                    }

        Http.GoodStatus_ metadata body ->
            case Bytes.Decode.decode decoder body of
                Nothing ->
                    Err <|
                        BadBody
                            { url = metadata.url
                            , statusCode = metadata.statusCode
                            , statusText = metadata.statusText
                            , headers = metadata.headers
                            , body = body
                            }
                            ""

                Just contents ->
                    Ok
                        { url = metadata.url
                        , statusCode = metadata.statusCode
                        , statusText = metadata.statusText
                        , headers = metadata.headers
                        , body = contents
                        }


rawBytesResolver : Http.Resolver (Error Bytes) (Response Bytes)
rawBytesResolver =
    Http.bytesResolver rawBytesParser


rawBytesParser :
    Http.Response Bytes
    -> Result (Error Bytes) (Response Bytes)
rawBytesParser response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err <|
                BadStatus
                    { url = metadata.url
                    , statusCode = metadata.statusCode
                    , statusText = metadata.statusText
                    , headers = metadata.headers
                    , body = body
                    }

        Http.GoodStatus_ metadata body ->
            Ok
                { url = metadata.url
                , statusCode = metadata.statusCode
                , statusText = metadata.statusText
                , headers = metadata.headers
                , body = body
                }
