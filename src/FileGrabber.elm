module FileGrabber exposing (expectBytes)

import Bytes exposing (Bytes)
import Bytes.Decode exposing (Decoder)
import Http
import Filename


expectBytes : (Result Http.Error Bytes -> msg) -> Http.Expect msg
expectBytes toMsg =
    Http.expectBytesResponse toMsg <|
        \response ->
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
                    Ok body
