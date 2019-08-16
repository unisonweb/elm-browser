module Ucb.Util.Http exposing (jsonResolver)

import Http
import Json.Decode as Decode exposing (Decoder, decodeString)


jsonResolver : Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    let
        parse : Http.Response String -> Result Http.Error a
        parse response =
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case decodeString decoder body of
                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))

                        Ok contents ->
                            Ok contents
    in
    Http.stringResolver parse
